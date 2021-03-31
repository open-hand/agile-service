import React, { Component } from 'react';
import { observer } from 'mobx-react';
import UserTag from '@/components/tag/user-tag';

@observer class FieldCreator extends Component {
  render() {
    const { store } = this.props;
    const issue = store.getIssue;
    const {
      createdBy, createrImageUrl,
      createrLoginName, createrName, createrRealName,
    } = issue;
    const field = store.getFieldByCode('created_user');
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            {field?.fieldName}
          </span>
        </div>
        <div className="c7n-value-wrapper" style={{ display: 'flex', flexWrap: 'nowrap', padding: '0 0.05rem' }}>
          {
              createdBy ? (
                <UserTag
                  data={{
                    id: createdBy,
                    loginName: createrLoginName,
                    realName: createrRealName,
                    imageUrl: createrImageUrl,
                    tooltip: createrName,
                  }}
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

export default FieldCreator;
