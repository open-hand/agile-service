import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Tooltip } from 'choerodon-ui/pro';
import TextEditToggle from '@/components/TextEditTogglePro';
import { featureApi } from '@/api';
import SelectProgramVersion from '@/components/select/select-program-version';

class FieldProgramVersion extends Component {
  renderItem(name, symbol = ',') {
    if (name && [...name].length > 20) {
      return (
        <Tooltip title={name}>
          <span>
            {name.substring(0, 20)}
            ...
          </span>
          {symbol}
        </Tooltip>
      );
    }
    return (
      <span>
        {name}
        {symbol}
      </span>
    );
  }

  updateIssueField = async (value) => {
    const {
      store, onUpdate, reloadIssue, field, setIssueLoading,
    } = this.props;
    const issue = store.getIssue;
    const { issueId, objectVersionNumber } = issue;
    setIssueLoading(true);
    await featureApi.updateVersions(issueId, value);
    if (onUpdate) {
      onUpdate();
    }
    await reloadIssue(issueId);
  };

  render() {
    const { store, disabled } = this.props;
    const issue = store.getIssue;
    const { programVersionFeatureRelVOS, activePiTeams } = issue;
    const field = store.getFieldByCode('programVersion');
    const required = field?.required;
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            版本
          </span>
        </div>
        <div className="c7n-value-wrapper" style={{ width: 'auto' }}>
          <TextEditToggle
            initValue={programVersionFeatureRelVOS && programVersionFeatureRelVOS.length > 0 ? programVersionFeatureRelVOS.map((item) => String(item.programVersionId)) : undefined}
            onSubmit={this.updateIssueField}
            alwaysRender={false}
            editor={() => (
              <SelectProgramVersion
                multiple
                projectId={store.projectId}
                required={required}
              />
            )}
            submitTrigger={['blur']}
            disabled={disabled}
          >

            {programVersionFeatureRelVOS && programVersionFeatureRelVOS.length > 0 ? (
              <p className="primary" style={{ wordBreak: 'break-word' }}>
                {
                  programVersionFeatureRelVOS.map((item, index, arr) => this.renderItem(item.name, index === arr.length - 1 ? '' : undefined))
                }
              </p>
            ) : <span>无</span>}
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default observer(FieldProgramVersion);
