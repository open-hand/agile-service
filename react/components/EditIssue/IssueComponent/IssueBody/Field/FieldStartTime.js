import React, { Component } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import { DateTimePicker } from 'choerodon-ui/pro';
import TextEditToggle from '@/components/TextEditTogglePro';
import { DatetimeAgo } from '../../../../CommonComponent';

class FieldStartTime extends Component {
  updateIssueField = (value) => {
    // const {
    //   store, onUpdate, reloadIssue, field,
    // } = this.props;
    // const issue = store.getIssue;
    // const {
    //   fieldId, fieldType,
    // } = field;
    // let newValue = value;
    // if (fieldType === 'time' || fieldType === 'datetime' || fieldType === 'date') {
    //   newValue = value && value.format('YYYY-MM-DD HH:mm:ss');
    // }
    // const { issueId } = issue;
    // const obj = {
    //   fieldType,
    //   value: newValue,
    // };
    // fieldApi.updateFieldValue(issueId, fieldId, 'agile_issue', obj)
    //   .then(() => {
    //     if (onUpdate) {
    //       onUpdate();
    //     }
    //     if (reloadIssue) {
    //       reloadIssue(issueId);
    //     }
    //   });
  };

  render() {
    const { store } = this.props;
    const issue = store.getIssue;
    const { creationDate } = issue;
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            预计开始时间
          </span>
        </div>
        <div className="c7n-value-wrapper" style={{ width: 'auto' }}>
          <TextEditToggle
            initValue={creationDate ? moment(creationDate) : undefined}
            onSubmit={this.updateIssueField}
            alwaysRender={false}
            editor={() => <DateTimePicker />}
            submitTrigger={['blur']}
          >
            <DatetimeAgo
              date={creationDate}
            />
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default observer(FieldStartTime);
