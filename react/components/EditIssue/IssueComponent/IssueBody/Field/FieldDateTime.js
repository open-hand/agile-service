import React, { Component } from 'react';
import { observer } from 'mobx-react';

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
        <div className="c7n-value-wrapper" style={{ padding: '0 0.05rem' }}>
          {value}
        </div>
      </div>
    );
  }
}

export default FieldDateTime;
