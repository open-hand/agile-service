import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Tooltip } from 'choerodon-ui/pro';
import { Permission } from '@choerodon/boot';
import SelectPI from '@/components/select/select-pi';
import TextEditToggle from '@/components/TextEditTogglePro';
import { piApi } from '@/api';

@observer
class FieldPI extends Component {
  updateIssuePI = async (value) => {
    const {
      store, onUpdate, reloadIssue, setIssueLoading,
    } = this.props;
    const issue = store.getIssue;
    const { issueId, activePi } = issue;
    const { id } = activePi || {};
    setIssueLoading(true);
    await piApi.project(store.projectId).addFeatures([issueId], id || 0, value || 0);
    if (onUpdate) {
      onUpdate();
    }
    await reloadIssue(issueId);
  }

  render() {
    const { store, disabled } = this.props;
    const issue = store.getIssue;
    const { activePi, closePi } = issue;
    const {
      name, code, id, statusCode, fullName,
    } = activePi || {};
    const field = store.getFieldByCode('pi');
    const required = field?.required;
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            PI
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <Permission
            type="project"
            projectId={store.projectId}
            service={[
              'choerodon.code.project.plan.feature.ps.choerodon.code.project.plan.feature.completepi',
              'choerodon.code.project.plan.feature.ps.choerodon.code.project.plan.feature.startpi',
              'choerodon.code.project.plan.feature.ps.pi.plan',
            ]}
          >
            {(hasPermission) => (
              <TextEditToggle
                disabled={(disabled) || (!hasPermission && statusCode === 'doing')}
                onSubmit={this.updateIssuePI}
                initValue={id}
                editor={({ submit }) => (
                  <SelectPI
                    required={required}
                    statusList={['todo', 'doing']}
                    multiple={false}
                    projectId={store.projectId}
                    allowClear
                    onChange={submit}
                    disabledCurrentPI={!hasPermission}
                  />
                )}
              >
                <Tooltip
                  placement="top"
                  title={`该特性经历PI数${closePi.length + (id ? 1 : 0)}`}
                >
                  <div>
                    {
                    !closePi.length && !id ? '无' : (
                      <div>
                        <div>
                          {closePi.map((p) => p.fullName || `${p.code}-${p.name}`).join(' , ')}
                        </div>
                        {
                          id && (
                            <div
                              style={{
                                color: '#4d90fe',
                                fontSize: '13px',
                                lineHeight: '20px',
                                display: 'inline-block',
                                marginTop: closePi.length ? 5 : 0,
                              }}
                            >
                              {fullName || `${code}-${name}`}
                            </div>
                          )
                        }
                      </div>
                    )
                  }
                  </div>
                </Tooltip>
              </TextEditToggle>
            )}
          </Permission>

        </div>
      </div>
    );
  }
}

export default FieldPI;
