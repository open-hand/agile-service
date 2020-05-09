import React, { Component } from 'react';
import { Icon, Popconfirm, Tooltip } from 'choerodon-ui';
import { stores, Permission } from '@choerodon/boot';
import { withRouter } from 'react-router-dom';
import { deleteIssue, updateIssue } from '../../../api/NewIssueApi';
import PriorityTag from '../../PriorityTag';
import StatusTag from '../../StatusTag';
import TypeTag from '../../TypeTag';
import UserHead from '../../UserHead';

const { AppState } = stores;

class IssueList extends Component {
  confirm = (issueId) => {
    this.handleDeleteIssue(issueId);
  };

  handleDeleteIssue(issueId) {
    const { onRefresh, issue: { objectVersionNumber, typeCode, createBy } } = this.props;
    const data = {
      issueId,
      relateIssueId: 0,
      objectVersionNumber,
    };
    if (typeCode === 'sub_task') {
      deleteIssue(issueId, createBy)
        .then(() => {
          if (onRefresh) {
            onRefresh();
          }
        });
    } else {
      updateIssue(data).then(() => {
        if (onRefresh) {
          onRefresh();
        }
      });
    }
  }

  getIssueTypeName(typeCode) {
    switch (typeCode) {
      case 'sub_task':
        return '子任务';
      case 'sub_bug':
        return '缺陷';
      default:
        return 'error';
    }
  }

  render() {
    const {
      issue, i, showAssignee, onOpen,
    } = this.props;
    const { typeCode } = issue;
    const menu = AppState.currentMenuType;
    const { type, id: projectId, organizationId: orgId } = menu;
    const issueTypeName = this.getIssueTypeName(issue.typeCode);
    return (
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          padding: '5px 0',
          cursor: 'pointer',
          borderBottom: '1px solid rgba(0, 0, 0, 0.12)',
          borderTop: !i ? '1px solid rgba(0, 0, 0, 0.12)' : '',
        }}
      >
        <Tooltip mouseEnterDelay={0.5} title={`任务类型: ${issueTypeName}`}>
          <div>
            <TypeTag
              data={issue.issueTypeVO}
            />
          </div>
        </Tooltip>
        <Tooltip title={`${issueTypeName}编号概要： ${issue.issueNum} ${issue.summary}`}>
          <div style={{ marginLeft: 8, flex: 1, overflow: 'hidden' }}>
            <p
              className="primary"
              style={{
                overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0,
              }}
              role="none"
              onClick={() => {
                onOpen(issue.issueId);
              }}
            >
              {`${issue.summary}`}
            </p>
          </div>
        </Tooltip>
        <div style={{ marginRight: '8px', overflow: 'hidden' }}>
          <Tooltip mouseEnterDelay={0.5} title={`优先级： ${issue.priorityVO.name}`}>
            <div>
              <PriorityTag
                priority={issue.priorityVO}
              />
            </div>
          </Tooltip>
        </div>
        {
          showAssignee ? (
            <div style={{ marginRight: 10, display: 'flex', justifyContent: 'flex-end' }}>
              <div>
                <UserHead
                  hiddenText
                  user={{
                    id: issue.assigneeId,
                    name: issue.assigneeName,
                    loginName: issue.loginName,
                    realName: issue.realName,
                    avatar: issue.imageUrl,
                  }}
                />
              </div>
            </div>
          ) : null
        }
        <div style={{
          marginRight: '8px', display: 'flex', justifyContent: 'flex-end',
        }}
        >
          <Tooltip mouseEnterDelay={0.5} title={`任务状态： ${issue.statusVO && issue.statusVO.name}`}>
            <div>
              <StatusTag
                data={issue.statusVO}
              />
            </div>
          </Tooltip>
        </div>
        <Permission type={type} projectId={projectId} organizationId={orgId} service={['agile-service.issue.deleteIssue']}>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              fontSize: '16px',
            }}
          >
            <Popconfirm
              title={`确认要删除该${issueTypeName}吗?`}
              placement="left"
              onConfirm={this.confirm.bind(this, issue.issueId)}
              onCancel={this.cancel}
              okText="删除"
              cancelText="取消"
              okType="danger"
            >
              <Icon type="delete_forever pointer" />
            </Popconfirm>
          </div>
        </Permission>
      </div>
    );
  }
}

export default withRouter(IssueList);
