import React, { Component } from 'react';
import { Icon, Popconfirm, Tooltip } from 'choerodon-ui';
import { issueLinkApi, featureApi } from '@/api';
import UserTag from '@/components/tag/user-tag';
import PriorityTag from '../../PriorityTag';
import StatusTag from '../../StatusTag';
import TypeTag from '../../TypeTag';

class LinkList extends Component {
  confirm(issueId) {
    const { onDeleteConfirm } = this.props;
    if (onDeleteConfirm) {
      onDeleteConfirm(issueId);
    } else {
      this.handleDeleteIssue(issueId);
    }
  }

  handleDeleteIssue(linkId) {
    const { onRefresh, issue, projectId } = this.props;
    const { typeCode } = issue;
    if (typeCode !== 'feature') {
      issueLinkApi.project(projectId).delete(linkId)
        .then(() => {
          onRefresh();
        });
    } else {
      featureApi.project(projectId).deleteLink(linkId)
        .then(() => {
          onRefresh();
        });
    }
  }

  render() {
    const {
      issue, i, showAssignee, showProject,
      canDelete = true, onOpen, deleteTipTitle: propsDeleteTipTitle,
    } = this.props;

    const { typeCode, starBeacon } = issue;
    let deleteTipTitle = propsDeleteTipTitle || '确认删除该工作项的关联关系吗';

    if (typeCode === 'feature') {
      deleteTipTitle = '确认要删除该特性关联关系吗?';
    }

    return (
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          padding: '5px 0',
          cursor: 'pointer',
          borderBottom: '1px solid var(--divider)',
          borderTop: !i ? '1px solid var(--divider)' : '',
        }}
      >
        <Tooltip mouseEnterDelay={0.5} title={`任务类型： ${issue.typeCode}`}>
          <div>
            <TypeTag
              data={issue.issueTypeVO}
            />
          </div>
        </Tooltip>
        <Tooltip title={`编号概要： ${issue.issueNum} ${issue.summary}`}>
          <div style={{
            marginLeft: 8, marginRight: 12, flex: 1, overflow: 'hidden',
          }}
          >
            <p
              className="primary"
              style={{
                overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0,
              }}
              role="none"
              onClick={() => {
                onOpen(issue.issueId, issue.linkedIssueId);
              }}
            >
              {`${issue.issueNum} ${issue.summary}`}
            </p>
          </div>
        </Tooltip>
        {/* <Star active={starBeacon} style={{ marginRight: 8 }} disabled /> */}
        {
          typeCode !== 'feature' && (
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
          typeCode === 'feature' && (
            <div style={{ marginRight: '8px', overflow: 'hidden' }}>
              <Tooltip mouseEnterDelay={0.5} title={`wsjf：${issue.wsjf}`}>
                <span style={{
                  width: '30px',
                  height: '20px',
                  backgroundColor: '#EBEBEB',
                  display: 'flex',
                  alignItems: 'center',
                  justifyContent: 'center',
                  borderRadius: '2px',
                  color: 'rgba(0, 0, 0, 0.85)',
                }}
                >
                  {(issue.wsjf || issue.wsjf === 0) ? issue.wsjf : '无'}
                </span>
              </Tooltip>
            </div>
          )
        }
        {
          typeCode !== 'feature' && showAssignee && issue.assigneeId ? (
            <Tooltip mouseEnterDelay={0.5} title={`${issue.assigneeName}`}>
              <div style={{
                marginRight: 10, display: 'flex', justifyContent: 'flex-end',
              }}
              >
                <div>
                  <UserTag
                    showText={false}
                    tooltip={false}
                    data={{
                      id: issue.assigneeId,
                      loginName: '',
                      realName: issue.assigneeName,
                      imageUrl: issue.imageUrl,
                    }}
                  />
                </div>
              </div>
            </Tooltip>
          ) : null
        }
        {/* {
          showProject && (
            <ProjectHead project={projects} hiddenText />
          )
        } */}
        <div style={{
          marginRight: '8px', display: 'flex', justifyContent: 'flex-end',
        }}
        >
          <Tooltip mouseEnterDelay={0.5} title={`任务状态： ${typeCode !== 'feature' ? (issue.statusVO && issue.statusVO.name) : (issue.statusMapVO && issue.statusMapVO.name)}`}>
            <div>
              <StatusTag
                data={typeCode !== 'feature' ? issue.statusVO : issue.statusMapVO}
              />
            </div>
          </Tooltip>
        </div>
        {canDelete
          ? (
            <div
              style={{
                display: 'flex',
                alignItems: 'center',
                fontSize: '16px',
              }}
            >
              <Popconfirm
                title={deleteTipTitle}
                placement="left"
                onConfirm={() => this.confirm(typeCode === 'feature' ? issue.featureDependId : issue.linkId)}
                onCancel={this.cancel}
                okText="删除"
                cancelText="取消"
                okType="danger"
              >
                <Icon type="delete_sweep-o" />
              </Popconfirm>
            </div>
          ) : ''}
      </div>
    );
  }
}

export default LinkList;
