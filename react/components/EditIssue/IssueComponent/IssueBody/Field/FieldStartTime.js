import React, { Component } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import { DateTimePicker } from 'choerodon-ui/pro';
import TextEditToggle from '@/components/TextEditTogglePro';
import { issueApi } from '@/api';
import { DatetimeAgo } from '../../../../CommonComponent';

class FieldStartTime extends Component {
  updateIssueField = (value) => {
    const {
      store, onUpdate, reloadIssue, field,
    } = this.props;
    const issue = store.getIssue;
    const { issueId, objectVersionNumber } = issue;
    const obj = {
      issueId,
      objectVersionNumber,
      estimatedStartTime: value && value.format('YYYY-MM-DD HH:mm:ss'),
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
    const { store } = this.props;
    const issue = store.getIssue;
    const { estimatedStartTime, estimatedEndTime } = issue;
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            预计开始时间
          </span>
        </div>
        <div className="c7n-value-wrapper" style={{ width: 'auto' }}>
          <TextEditToggle
            initValue={estimatedStartTime ? moment(estimatedStartTime) : undefined}
            onSubmit={this.updateIssueField}
            alwaysRender={false}
            editor={() => <DateTimePicker max={estimatedEndTime && moment(estimatedEndTime).subtract(1, 's')} />}
            submitTrigger={['blur']}
          >
            {
              estimatedStartTime ? (
                <DatetimeAgo
                  date={estimatedStartTime}
                />
              ) : '无'
            }
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default observer(FieldStartTime);
