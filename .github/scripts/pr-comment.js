module.exports = async({ github, context }) => {

    console.log(`Workflow: ${JSON.stringify(context.payload.workflow_run,null,2)}`);

    /* We use this link service as github does not (yet) expose an API where
       you can download an artifact.
       https://github.com/actions/upload-artifact/issues/50
    */
    var nightlyURL = (artifact) => {
        return `https://nightly.link/${context.repo.owner}/${context.repo.repo}/actions/artifacts/${artifact.id}.zip`
    };

    const artifacts = await github.paginate(
        github.rest.actions.listWorkflowRunArtifacts,
        {
            owner: context.repo.owner,
            repo: context.repo.repo,
            run_id: context.payload.workflow_run.id,
            per_page: 100
        });

    const ct_logs = artifacts.find(
        (a) => { return a.name == 'test_results'; });
    const html_docs = artifacts.find(
        (a) => { return a.name == 'otp_doc_html'; });
    const win_exe = artifacts.find(
        (a) => { return a.name == 'otp_win32_installer'; });

    let gh_comments = await github.paginate(
        github.rest.issues.listComments,
        {
            owner: context.repo.owner,
            repo: context.repo.repo,
            issue_number: context.payload.workflow_run.pull_requests[0].number,
            per_page: 100
        });

    /* We find the newest comment by "github-actions[bot]". There really should
       only be one, but the PR scripts may be buggy and if so we want to updated
       the latest post. */
    gh_comments.reverse();
    const gh_comment = gh_comments.find(c => c.user.login == "github-actions[bot]");

    console.log(`Comment to update: ${JSON.stringify(gh_comment,null,2)}`);

    let ct_body;

    /* The EnricoMi/publish-unit-test-result-action@v1 action creates/updates a comment
       to always start with "## CT Test Results". Below we append some data to that
       comment to further help the user.
    */

    if (gh_comment) {
        /* If the comment has a marker, it has not been touched by
           EnricoMi/publish-unit-test-result-action@v1 and then we do not need
           to update the comment */
        if (gh_comment.body.match("<!-- marker -->"))
            return;
        ct_body = gh_comment.body;
    } else {
        /* if the comment does not exist we use a place holder comment. This
           needs to start with "## CT Test Results" as otherwise
           EnricoMi/publish-unit-test-result-action@v1 will create a new comment. */
        ct_body = `## CT Test Results

Tests are running... ${context.payload.workflow_run.html_url}

Results for commit ${context.payload.workflow_run.head_sha}`;
    }

    console.log(`ct_body: ${ct_body}`);

    const body = `${ct_body}

<!-- split -->

To speed up review, make sure that you have read [Contributing to Erlang/OTP](https://github.com/erlang/otp/blob/master/CONTRIBUTING.md) and that all [checks](/${context.repo.owner}/${context.repo.repo}/pull/${ context.issue.number }/checks) pass.

See the [TESTING](https://github.com/erlang/otp/blob/master/HOWTO/TESTING.md) and [DEVELOPMENT](https://github.com/erlang/otp/blob/master/HOWTO/DEVELOPMENT.md) HowTo guides for details about how to run test locally.

## Artifacts
* ` + (ct_logs ? `[Complete CT logs](${nightlyURL(ct_logs)})` : "No CT logs found") + `
* ` + (html_docs ? `[HTML Documentation](${nightlyURL(html_docs)})` : "No HTML docs found") + `
* ` + (win_exe ? `[Windows Installer](${nightlyURL(win_exe)})` : "No Windows Installer found") + `

// Erlang/OTP Github Action Bot
`;
    if (gh_comment) {
        if (gh_comment.body != body) {
            console.log("Update comment: " + gh_comment.id);
            await github.rest.issues.updateComment({
                owner: context.repo.owner,
                repo: context.repo.repo,
                comment_id: gh_comment.id,
                body: body
            });
        }
    } else {
        await github.rest.issues.createComment({
            owner: context.repo.owner,
            repo: context.repo.repo,
            issue_number: context.payload.workflow_run.pull_requests[0].number,
            body: body
        });
    }
};
