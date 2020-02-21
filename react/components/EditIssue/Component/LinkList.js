import React, { Component } from 'react';
import { Icon, Popconfirm, Tooltip } from 'choerodon-ui';
import UserHead from '../../UserHead';
import { deleteLink } from '../../../api/NewIssueApi';
import PriorityTag from '../../PriorityTag';
import StatusTag from '../../StatusTag';
import TypeTag from '../../TypeTag';

class LinkList extends Component {
  confirm(issueId) {
    this.handleDeleteIssue(issueId);
  }

  handleDeleteIssue(linkId) {
    const { onRefresh } = this.props;
    deleteLink(linkId)
      .then(() => {
        onRefresh();
      });
  }

  render() {
    const {
      issue, i, showAssignee, showProject,
      canDelete = true, onOpen, type,
    } = this.props;

    const { typeCode } = issue;

    let deleteTipTitle = '确认要删除该问题链接吗？';
    if (type === 'test') {
      deleteTipTitle = '确认要删除该测试用例吗?';
    } else {
      deleteTipTitle = '确认要删除该特性关联关系吗?';
    }

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
        {
          typeCode !== 'feature' && (
          <div style={{ marginRight: '8px', overflow: 'hidden' }}>
            <Tooltip mouseEnterDelay={0.5} title={`优先级： ${issue.priorityVO.name}`}>
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
          showAssignee ? (
            <Tooltip mouseEnterDelay={0.5} title={`经办人： ${issue.assigneeName}`}>
              <div style={{
                marginRight: 29, display: 'flex', justifyContent: 'flex-end', 
              }}
              >
                <div>
                  <UserHead
                    user={{
                      id: issue.assigneeId,
                      loginName: '',
                      realName: issue.assigneeName,
                      avatar: issue.imageUrl,
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
          width: '48px', marginRight: '8px', display: 'flex', justifyContent: 'flex-end', 
        }}
        >
          <Tooltip mouseEnterDelay={0.5} title={`任务状态： ${issue.statusVO.name}`}>
            <div>
              <StatusTag
                data={issue.statusVO}
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
                onConfirm={this.confirm.bind(this, issue.linkId)}
                onCancel={this.cancel}
                okText="删除"
                cancelText="取消"
                okType="danger"
              >
                <Icon type="delete_forever pointer" />
              </Popconfirm>
            </div>
          ) : ''
        }
      </div>
    );
  }
}

export default LinkList;
