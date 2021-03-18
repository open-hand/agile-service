import React, { Component } from 'react';
import { observer } from 'mobx-react';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectAppVersion from '@/components/select/select-app-version';
import { issueApi } from '@/api';

@observer class FieldAppVersion extends Component {
  updateIssueField = (value) => {
    const {
      store, onUpdate, field, reloadIssue,
    } = this.props;
    const issue = store.getIssue;

    const { issueId, objectVersionNumber } = issue;
    const obj = {
      issueId,
      objectVersionNumber,
      appVersions: value ? value.map((i) => ({ id: i })) : [],
    };
    issueApi.update(obj)
      .then(() => {
        if (onUpdate) {
          onUpdate();
        }
        if (reloadIssue) {
          reloadIssue(issueId);
        }
      });
  };

  render() {
    const { store, disabled } = this.props;
    const issue = store.getIssue;
    const { appVersions } = issue;
    const field = store.getFieldByCode('app_version');
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            {field?.fieldName}
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <TextEditToggle
            disabled={disabled}
            onSubmit={this.updateIssueField}
            initValue={appVersions.map((i) => i.id) || undefined}
            editor={({ submit }) => (
              <SelectAppVersion />
            )}
          >
            {appVersions && appVersions.length > 0
              ? appVersions.map((i) => `${i.artifactId}/${i.versionAlias || i.version}`).join('、') : '无'}
          </TextEditToggle>

        </div>
      </div>
    );
  }
}

export default FieldAppVersion;
