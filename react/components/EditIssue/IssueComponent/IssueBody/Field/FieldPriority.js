import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import SelectPriority from '@/components/select/select-priority';
import TextEditToggle from '@/components/TextEditTogglePro';
import { issueApi } from '@/api';

@inject('AppState')
@observer class FieldPriority extends Component {
  updateIssuePriority = (newPriorityId) => {
    const { store } = this.props;
    const issue = store.getIssue;
    const { issueId, objectVersionNumber } = issue;
    const obj = {
      issueId,
      objectVersionNumber,
      priorityId: newPriorityId,
    };
    store.update(obj);
  };

  render() {
    const { store, disabled } = this.props;
    const issue = store.getIssue;
    const { priorityId, priorityVO = {} } = issue;
    const { colour, name } = priorityVO || {}; // 防止优先级为空时 出错
    const field = store.getFieldByCode('priority');
    const required = field?.required || store.getRuleRequired(field);
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            优先级
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <TextEditToggle
            disabled={disabled}
            editor={({ submit }) => (
              <SelectPriority
                required={required}
                projectId={store.projectId}
                priorityId={priorityId}
                onChange={submit}
                {
                  ...store.getOptionsData(field, priorityId)
                }
              />
            )}
            initValue={priorityId}
            onSubmit={this.updateIssuePriority}
          >
            {priorityId ? (
              <div
                className="c7n-level"
                style={{
                  backgroundColor: `${colour}1F`,
                  color: colour,
                  borderRadius: '2px',
                  padding: '0 8px',
                  display: 'inline-block',
                }}
              >
                {name}
              </div>
            ) : (
              <div>
                无
              </div>
            )}
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldPriority));
