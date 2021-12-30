module.exports = async({ github, context, body }) => {

    /* We use this link service as github does not (yet) expose an API where
       you can download an artifact.
       https://github.com/actions/upload-artifact/issues/50
    */
    var nightlyURL = (artifact) => {
        return `https://nightly.link/${context.repo.owner}/${context.repo.repo}/actions/artifacts/${artifact.id}.zip`
    };

    const artifacts = await github.rest.actions.listWorkflowRunArtifacts({
        owner: context.repo.owner,
        repo: context.repo.repo,
        run_id: context.payload.workflow_run.id,
        per_page: 100
    });

    const ct_logs = artifacts.data.artifacts.find(
        (a) => { return a.name == 'test_results'; });
    const html_docs = artifacts.data.artifacts.find(
        (a) => { return a.name == 'otp_doc_html'; });
    const win_exe = artifacts.data.artifacts.find(
        (a) => { return a.name == 'otp_win32_installer'; });

    // If the body already contains an Artifacts header, this is a call to
    // update the comment without having updated any test results. So
    // We need to remove the intro and extro from the body
    if (/##/.test(body)) {
        body = body.split("##")[1];
    }

    return `Hello!

Thanks for opening a PR to improve Erlang/OTP.

To speed up review, make sure that you have read [Contributing to Erlang/OTP](https://github.com/erlang/otp/blob/master/CONTRIBUTING.md) and that all [checks](/${{ github.repository }}/pull/${{ github.event.workflow_run.pull_requests[0].number }}/checks) pass.

${body}

## Artifacts
* ` + (ct_logs ? `[Complete CT logs](${nightlyURL(ct_logs)})` : "No CT logs found") + `
* ` + (html_docs ? `[HTML Documentation](${nightlyURL(html_docs)})` : "No HTML docs found") + `
* ` + (win_exe ? `[Windows Installer](${nightlyURL(win_exe)})` : "No Windows Installer found") + `

See the [TESTING](https://github.com/erlang/otp/blob/master/HOWTO/TESTING.md) and [DEVELOPMENT](https://github.com/erlang/otp/blob/master/HOWTO/DEVELOPMENT.md) HowTo guides for details about how to run test locally.

// Erlang/OTP Github Action Bot
`;
};
