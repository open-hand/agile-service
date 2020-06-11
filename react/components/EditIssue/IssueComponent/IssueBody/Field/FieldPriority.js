import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import SelectPriority from '@/components/select/select-priority';
import TextEditTogglePro from '@/components/TextEditTogglePro';
import { updateIssue } from '../../../../../api/NewIssueApi';


@inject('AppState')
@observer class FieldPriority extends Component {
  updateIssuePriority = (newPriorityId) => {
    const { store, onUpdate, reloadIssue } = this.props;
    const issue = store.getIssue;
    const { issueId, objectVersionNumber } = issue;
    const obj = {
      issueId,
      objectVersionNumber,
      priorityId: newPriorityId,
    };
    updateIssue(obj)
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
    const { store, disabled } = this.props;
    const issue = store.getIssue;
    const { priorityId, priorityVO = {} } = issue;
    const { colour, name } = priorityVO || {}; // 防止优先级为空时 出错
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            优先级
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <TextEditTogglePro 
            disabled={disabled}
            renderText={() => (priorityId ? (
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
            ))}
            initValue={priorityId}
            onSubmit={this.updateIssuePriority}
          >
            <SelectPriority priorityId={priorityId} />
          </TextEditTogglePro>
          {/* <TextEditToggle
            disabled={disabled}
            formKey="priority"
            onSubmit={this.updateIssuePriority}
            originData={priorityId}
          >
            <Text>
              {
                priorityId ? (
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
                )
              }
            </Text>
            <Edit>
              <Select
                dropdownStyle={{ minWidth: 185 }}              
                loading={selectLoading}
                getPopupContainer={() => document.getElementById('detail')}
                onChange={(value) => {
                  this.setState({
                    newPriorityId: value,
                  });
                }}
                onFocus={() => {
                  this.loadIssuePriorities();
                }}
              >
                {
                  originPriorities.filter(p => p.enable || p.id === priorityId).map(priority => (
                    <Option key={priority.id} value={priority.id}>
                      <div style={{ display: 'inline-flex', alignItems: 'center', padding: '2px' }}>
                        <div
                          className="c7n-level"
                          style={{
                            borderRadius: '2px',
                            padding: '0 8px',
                            display: 'inline-block',
                          }}
                        >
                          {priority.name}
                        </div>
                      </div>
                    </Option>
                  ))
                }
              </Select>
            </Edit>
          </TextEditToggle> */}
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldPriority));
