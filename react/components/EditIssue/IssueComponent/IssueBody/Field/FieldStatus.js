import React, { Component, useMemo, forwardRef } from 'react';
import { observer } from 'mobx-react';
import { Select } from 'choerodon-ui/pro';
import STATUS from '@/constants/STATUS';
import { issueApi, statusApi } from '@/api';
import TextEditToggle from '@/components/TextEditTogglePro';
import useSelect from '@/hooks/useSelect';

const { Option } = Select;
const SelectStatus = forwardRef(({ statusArgs, ...otherProps }, ref) => {
  const {
    statusId, issueId, typeId, applyType, projectId, name,
  } = statusArgs;
  const config = useMemo(() => ({
    name: 'status',
    request: () => statusApi.project(projectId).loadTransformStatusByIssue(
      statusId,
      issueId,
      typeId,
      applyType,
      projectId,
    ),
    middleWare: (data) => {
      const newData = [...data];
      const currentStatus = (newData || []).find((item) => item.endStatusId === statusId);
      if (!currentStatus) {
        newData.unshift({
          id: 'current',
          name,
          endStatusId: statusId,
          statusVO: {
            name,
          },
        });
      }
      return newData;
    },
    paging: false,
    textField: 'statusVO.name',
    valueField: 'endStatusId',
  }), [JSON.stringify(statusArgs)]);

  const props = useSelect(config);
  return (
    <Select
      ref={ref}
      primitiveValue={false}
      clearButton={false}
      {...props}
      {...otherProps}
    />
  );
});
@observer class FieldStatus extends Component {
  updateIssueStatus = (transform) => {
    if (transform) {
      const transformId = transform.id;
      const {
        store, onUpdate, reloadIssue, applyType,
      } = this.props;
      const issue = store.getIssue;
      const { issueId, statusId, objectVersionNumber } = issue;
      if (statusId === transform.endStatusId) {
        return;
      }
      if (transformId) {
        issueApi.updateStatus(transformId, issueId, objectVersionNumber, applyType)
          .then(() => {
            if (onUpdate) {
              onUpdate();
            }
            if (reloadIssue) {
              reloadIssue(issueId);
            }
          });
      }
    }
  };

  render() {
    const {
      store, disabled, projectId, applyType, issueId: currentIssueId,
    } = this.props;
    const issue = store.getIssue;
    const {
      statusVO = {}, statusId, issueTypeVO = {},
      issueId,
    } = issue;
    const { type, name } = statusVO || {};
    const typeId = issueTypeVO.id;
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            状态
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <TextEditToggle
            disabled={disabled}
            onSubmit={this.updateIssueStatus}
            initValue={statusId}
            submitTrigger={['blur', 'change']}
            editor={() => (
              <SelectStatus
                key={`edit-issue-select-status-statusId-${statusId}`}
                statusArgs={{
                  statusId, issueId, typeId, projectId, applyType, name,
                }}
              />
            )}
          >
            {
              statusId ? (
                <div
                  style={{
                    background: STATUS[type],
                    color: '#fff',
                    borderRadius: '2px',
                    padding: '0 8px',
                    display: 'inline-block',
                    margin: '2px auto 2px 0',
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
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default FieldStatus;
