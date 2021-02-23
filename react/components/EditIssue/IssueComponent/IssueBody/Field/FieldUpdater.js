import React, { Component } from 'react';
import { observer } from 'mobx-react';
import UserHead from '../../../../UserHead';

@observer class FieldUpdater extends Component {
  render() {
    const { store } = this.props;
    const issue = store.getIssue;
    const { updater } = issue;
    const field = store.getFieldByCode('updator');
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            {field?.fieldName}
          </span>
        </div>
        <div className="c7n-value-wrapper" style={{ display: 'flex', flexWrap: 'nowrap' }}>
          {
              updater ? (
                <UserHead
                  user={updater}
                />
              ) : (
                <div>
                  无
                </div>
              )
            }
        </div>
      </div>
    );
  }
}

export default FieldUpdater;
