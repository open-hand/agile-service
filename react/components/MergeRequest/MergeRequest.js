import React, { Component } from 'react';
import {
  Table, Tooltip, Popover, Button, Icon,
} from 'choerodon-ui';
import {
  Choerodon,
} from '@choerodon/boot';
import { devOpsApi } from '@/api';
import './MergeRequest.less';
import UserTag from '../tag/user-tag';

const STATUS_SHOW = {
  opened: '开放',
  merged: '已合并',
  closed: '关闭',
};

class MergeRequest extends Component {
  constructor(props) {
    super(props);
    this.state = {
      mergeRequests: [],
      loading: false,
    };
  }

  componentDidMount() {
    this.loadMergeRequest();
  }

  loadMergeRequest() {
    const { issueId, projectId } = this.props;
    this.setState({ loading: true });
    devOpsApi.project(projectId).loadMergeRequest(issueId).then((res) => {
      this.setState({
        mergeRequests: res,
        loading: false,
      });
    });
  }

  createMergeRequest(record) {
    const win = window.open('');
    const { applicationId, gitlabMergeRequestId, projectId } = record;
    devOpsApi.project(projectId).loadGitUrl(applicationId).then((res) => {
      const url = `${res}/merge_requests/${gitlabMergeRequestId}`;
      win.location.href = url;
    })
      .catch((error) => {
      });
  }

  render() {
    const column = [
      {
        title: '名称',
        dataIndex: 'title',
        width: 180,
        fixed: 'left',
        render: (title, record) => (
          <div style={{ width: '100%', overflow: 'hidden', flexShrink: 0 }}>
            <Tooltip placement="topLeft" mouseEnterDelay={0.5} title={title}>
              <p
                role="none"
                className="c7n-agile-table-cell-click"
                style={{
                  overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0,
                }}
                onClick={this.createMergeRequest.bind(this, record)}
              >
                {title}
              </p>
            </Tooltip>
          </div>
        ),
      },
      {
        title: '编码',
        dataIndex: 'gitlabMergeRequestId',
        width: 100,
        render: (id, record) => (
          <div style={{ width: '100%', overflow: 'hidden' }}>
            <Tooltip placement="topLeft" mouseEnterDelay={0.5} title={id}>
              <p
                role="none"
                style={{
                  overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0,
                }}
              >
                {`!${id}`}
              </p>
            </Tooltip>
          </div>
        ),
      },

      {
        title: '分支',
        dataIndex: 'sourceBranch',
        width: 250,
        render: (sourceBranch, record) => (
          <div style={{
            display: 'flex', alignItems: 'center', whiteSpace: 'nowrap',
          }}
          >
            <Tooltip title={sourceBranch}>
              <div style={{ overflow: 'hidden', textOverflow: 'ellipsis', minWidth: 100 }}>
                <Icon type="branch" />
                {sourceBranch}
              </div>
            </Tooltip>
            <Icon type="trending_flat" />
            <Tooltip title={record.targetBranch}>
              <div style={{ overflow: 'hidden', textOverflow: 'ellipsis', minWidth: 100 }}>
                <Icon type="branch" />
                {record.targetBranch}
              </div>
            </Tooltip>
          </div>
        ),
      },
      {
        title: '状态',
        dataIndex: 'state',
        width: 70,
        render: (state) => (
          <div style={{ width: '100%', overflow: 'hidden', flexShrink: 0 }}>
            {['opened', 'merged', 'closed'].includes(state) ? STATUS_SHOW[state] : ''}
          </div>
        ),
      },
      {
        title: '审查人',
        dataIndex: 'authorId',
        width: 100,
        render: (authorId, record) => (
          <div style={{
            width: '100%', overflow: 'hidden', flexShrink: 0, justifyContent: 'flex-start',
          }}
          >
            <UserTag
              data={{
                id: record.assigneeId,
                tooltip: record.assigneeName,
                realName: record.assigneeName,
                imageUrl: record.assigneeImageUrl,
              }}
            />
          </div>
        ),
      },
      {
        title: '更新时间',
        dataIndex: 'updatedAt',
        width: 100,
        render: (updatedAt) => (
          <div style={{ width: '100%', overflow: 'hidden', flexShrink: 0 }}>
            <Popover
              title="更新时间"
              content={updatedAt}
              placement="left"
            >
              {updatedAt}
            </Popover>
          </div>
        ),
      },
    ];
    return (
      <Table
        className="c7nagile-MergeRequest"
        pagination={false}
        filterBar={false}
        rowKey={(record) => record.id}
        columns={column}
        dataSource={this.state.mergeRequests}
        loading={this.state.loading}
        scroll={{ x: true }}
      />
    );
  }
}
export default MergeRequest;
