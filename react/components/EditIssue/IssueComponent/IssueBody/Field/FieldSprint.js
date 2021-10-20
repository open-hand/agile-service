import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { Select, Tooltip } from 'choerodon-ui';
import { injectIntl } from 'react-intl';
import _ from 'lodash';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectSprint from '@/components/select/select-sprint';
import { sprintApi, issueApi } from '@/api';

const { Option } = Select;

@inject('AppState')
@observer class FieldSprint extends Component {
  updateIssueSprint = (newSprintId) => {
    const {
      store,
    } = this.props;
    const issue = store.getIssue;
    const { activeSprint = {}, issueId, objectVersionNumber } = issue;
    const sprintId = activeSprint ? activeSprint.sprintId : undefined;

    if (newSprintId !== sprintId) {
      const obj = {
        issueId,
        objectVersionNumber,
        sprintId: newSprintId || 0,
      };
      store.update(obj);
    }
  };

  render() {
    const { store, disabled } = this.props;
    const issue = store.getIssue;
    const { closeSprint = [], activeSprint = {} } = issue;
    const sprintId = activeSprint ? activeSprint.sprintId : undefined;
    const field = store.getFieldByCode('sprint');
    const required = field?.required;
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            冲刺
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <TextEditToggle
            disabled={disabled}
            onSubmit={this.updateIssueSprint}
            initValue={sprintId}
            editorExtraContent={
              closeSprint.length ? (
                <div style={{ maxWidth: 170 }}>
                  <span>已结束冲刺</span>
                  <span>
                    {_.map(closeSprint, 'sprintName').join(' , ')}
                  </span>
                </div>
              ) : null
            }
            editor={({ submit }) => <SelectSprint projectId={store.projectId} required={required} onChange={submit} />}
          >
            <Tooltip
              placement="top"
              title={`该工作项经历迭代数${closeSprint.length + (sprintId ? 1 : 0)}`}
            >
              <div style={{
                width: '100%',
                wordBreak: 'break-all',
              }}
              >
                {
                  !closeSprint.length && !sprintId ? '无' : (
                    <div>
                      <div>
                        {_.map(closeSprint, 'sprintName').join(' , ')}
                      </div>
                      {
                        sprintId && (
                          <div
                            style={{
                              color: '#4d90fe',
                              fontSize: '13px',
                              lineHeight: '20px',
                              display: 'inline-block',
                              marginTop: closeSprint.length ? 5 : 0,
                            }}
                          >
                            {activeSprint.sprintName}
                          </div>
                        )
                      }
                    </div>
                  )
                }
              </div>
            </Tooltip>
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldSprint));
