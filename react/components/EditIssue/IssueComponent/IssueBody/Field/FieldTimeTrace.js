import React, { Component } from 'react';
import { observer } from 'mobx-react';
import _ from 'lodash';
import openRecordWorkLogModal from '@/components/DailyLog/DailyLogPro';
/**
 * 精确加法
 */
function add(num1, num2) {
  const num1Digits = (num1.toString().split('.')[1] || '').length;
  const num2Digits = (num2.toString().split('.')[1] || '').length;
  const baseNum = Math.pow(10, Math.max(num1Digits, num2Digits));
  return (num1 * baseNum + num2 * baseNum) / baseNum;
}
@observer class FieldTimeTrace extends Component {
  getWorkloads = () => {
    const { store } = this.props;
    const worklogs = store.getWorkLogs;
    if (!Array.isArray(worklogs)) {
      return 0;
    }
    return _.reduce(worklogs, (sum, v) => add(sum, (v.workTime || 0)), 0);
  };

  render() {
    const {
      store, disabled, issueId, reloadIssue,
    } = this.props;
    const issue = store.getIssue;
    const { remainingTime } = issue;
    const workloads = this.getWorkloads();
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            时间跟踪
          </span>
        </div>
        <div className="c7n-value-wrapper" style={{ paddingLeft: '0.05rem' }}>
          <span style={{ flex: 1 }}>
            {workloads}
            {'小时/'}
            {workloads + (remainingTime || 0)}
            {'小时'}
          </span>
          {
            !disabled && (
              <div
                role="none"
                className="primary"
                style={{
                  margin: '0 10px',
                  cursor: 'pointer',
                }}
                onClick={() => {
                  openRecordWorkLogModal({ issueId, onOk: () => reloadIssue(issueId) });
                  // store.setWorkLogShow(true);
                }}
              >
                登记工作
              </div>
            )
          }
        </div>
      </div>
    );
  }
}

export default FieldTimeTrace;
