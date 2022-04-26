import React, { Component } from 'react';
import { Icon, Popconfirm, Tooltip } from 'choerodon-ui';
import { stores, Permission } from '@choerodon/boot';
import { withRouter } from 'react-router-dom';
import EditIssueContext from '../stores';
import { issueApi } from '@/api';
import Star from '@/components/tag/star';
import UserTag from '@/components/tag/user-tag';
import PriorityTag from '../../PriorityTag';
import StatusTag from '../../StatusTag';
import TypeTag from '../../TypeTag';

const { AppState } = stores;

class IssueList extends Component {
  confirm = (issueId) => {
    this.handleDeleteIssue(issueId);
  };

  handleDeleteIssue(issueId) {
    const { store } = this.context;
    const { onRefresh, issue: { objectVersionNumber, typeCode, createdBy } } = this.props;
    const data = {
      issueId,
      relateIssueId: 0,
      objectVersionNumber,
    };
    if (typeCode === 'sub_task') {
      issueApi.project(store.projectId).delete(issueId, createdBy)
        .then(() => {
          if (onRefresh) {
            onRefresh();
          }
        });
    } else {
      issueApi.project(store.projectId).update(data).then(() => {
        if (onRefresh) {
          onRefresh(issueId);
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
      issue, i, showAssignee, showDelete, showPriority, onOpen, style,
    } = this.props;
    const { store } = this.context;
    const { typeCode, starBeacon } = issue;
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
          borderBottom: '1px solid var(--divider)',
          borderTop: !i ? '1px solid var(--divider)' : '',
          ...style,
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
              className="c7n-issueList-summary"
              style={{
                color: '#5365EA', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0,
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
        <Star disabled active={starBeacon} style={{ margin: '0 8px' }} />
        {
          showPriority && (
            <div style={{ marginRight: '8px', overflow: 'hidden' }}>
              <Tooltip mouseEnterDelay={0.5} title={`优先级： ${issue.priorityVO?.name}`}>
                <div>
                  <PriorityTag
                    priority={issue.priorityVO}
                  />
                </div>
              </Tooltip>
            </div>
          )
        }
        {
          showAssignee && issue.assigneeId ? (
            <div style={{ marginRight: 10, display: 'flex', justifyContent: 'flex-end' }}>
              <div>
                <UserTag
                  showText={false}
                  data={{
                    id: issue.assigneeId,
                    tooltip: issue.assigneeName,
                    loginName: issue.loginName,
                    realName: issue.realName,
                    imageUrl: issue.imageUrl,
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
        {
          showDelete && (
            <Permission type="project" projectId={store.projectId} service={['choerodon.code.project.cooperation.iteration-plan.ps.choerodon.code.agile.project.editissue.pro']}>
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
                  // eslint-disable-next-line react/jsx-no-bind
                  onConfirm={this.confirm.bind(this, issue.issueId)}
                  onCancel={this.cancel}
                  okText="删除"
                  cancelText="取消"
                  okType="danger"
                >
                  <Icon type="delete_sweep-o" />
                </Popconfirm>
              </div>
            </Permission>
          )
        }
      </div>
    );
  }
}
IssueList.contextType = EditIssueContext;
export default withRouter(IssueList);
