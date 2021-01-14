import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { DatetimeAgo } from '../../../../CommonComponent';

@observer class FieldDateTime extends Component {
  render() {
    const { store, field } = this.props;
    const { fieldCode, fieldName } = field;
    const issue = store.getIssue;
    const { [fieldCode]: value } = issue;
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            {`${fieldName}`}
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <DatetimeAgo
            date={value}
          />
        </div>
      </div>
    );
  }
}

export default FieldDateTime;
