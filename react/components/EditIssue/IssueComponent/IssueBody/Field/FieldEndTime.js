import React, { Component } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import { DateTimePicker } from 'choerodon-ui/pro';
import TextEditToggle from '@/components/TextEditTogglePro';
import { toJS } from 'mobx';
import { DatetimeAgo } from '../../../../CommonComponent';

class FieldEndTime extends Component {
  updateIssueField = (value) => {
    // console.log('newEndTime:');
    // console.log(value && value.format('YYYY-MM-DD HH:mm:ss'));
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
    // console.log('issue：');
    // console.log(toJS(issue));
    const { lastUpdateDate, creationDate } = issue;
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            预计结束时间
          </span>
        </div>
        <div className="c7n-value-wrapper" style={{ width: 'auto' }}>
          <TextEditToggle
            initValue={lastUpdateDate ? moment(lastUpdateDate) : undefined}
            onSubmit={this.updateIssueField}
            alwaysRender={false}
            editor={() => <DateTimePicker min={creationDate && moment(creationDate).add(1, 's')} />}
            submitTrigger={['blur']}
          >
            <DatetimeAgo
              date={lastUpdateDate}
            />
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default observer(FieldEndTime);
