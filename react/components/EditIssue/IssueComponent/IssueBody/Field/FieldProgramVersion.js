import React, { Component } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import { DateTimePicker, Select, Tooltip } from 'choerodon-ui/pro';
import TextEditToggle from '@/components/TextEditTogglePro';
import { toJS } from 'mobx';
import { issueApi } from '@/api';

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

  updateIssueField = (value) => {
    const {
      store, onUpdate, reloadIssue, field,
    } = this.props;
    const issue = store.getIssue;
    const { issueId, objectVersionNumber } = issue;
    const obj = {
      issueId,
      objectVersionNumber,
      programVersionFeatureRelVOS: value,
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
    const { programVersionFeatureRelVOS } = issue;
    const field = store.getFieldByCode('estimatedEndTime');
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
            initValue={programVersionFeatureRelVOS && programVersionFeatureRelVOS.length > 0 ? programVersionFeatureRelVOS.map((item) => item.id) : undefined}
            onSubmit={this.updateIssueField}
            alwaysRender={false}
            editor={() => <Select required={required} />}
            submitTrigger={['blur']}
            disabled={disabled}
          >

            <p className="primary" style={{ wordBreak: 'break-word' }}>
              {programVersionFeatureRelVOS && programVersionFeatureRelVOS.length > 0
                ? programVersionFeatureRelVOS.map((item, index, arr) => this.renderItem(item.name, index === arr.length - 1 ? '' : undefined)) : '无'}

            </p>

          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default observer(FieldProgramVersion);
