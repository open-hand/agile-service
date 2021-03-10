import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { Progress } from 'choerodon-ui';
import { injectIntl } from 'react-intl';
import _ from 'lodash';

@observer class FieldTimeTrace extends Component {
  getWorkloads = () => {
    const { store } = this.props;
    const worklogs = store.getWorkLogs;
    if (!Array.isArray(worklogs)) {
      return 0;
    }
    return _.reduce(worklogs, (sum, v) => sum + (v.workTime || 0), 0);
  };

  render() {
    const { store, disabled } = this.props;
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
                  store.setWorkLogShow(true);
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
