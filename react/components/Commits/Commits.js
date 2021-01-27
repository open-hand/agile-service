/* eslint-disable react/jsx-no-bind */
/* eslint-disable no-restricted-globals */
import React, { Component } from 'react';
import _ from 'lodash';
import {
  Modal, Table, Tooltip, Popover, Button, Icon, Menu,
} from 'choerodon-ui';
import {
  Dropdown,
} from 'choerodon-ui/pro';
import { devOpsApi } from '@/api';

const STATUS_SHOW = {
  opened: '开放',
  merged: '已合并',
  closed: '关闭',
};

class Commits extends Component {
  constructor(props) {
    super(props);
    this.state = {
      commits: [],
      loading: false,
    };
  }

  componentDidMount() {
    this.loadCommits();
  }

  getStatus(mergeRequests) {
    if (!mergeRequests.length) {
      return '';
    }
    const statusArray = _.map(mergeRequests, 'state');
    if (statusArray.includes('opened')) {
      return '开放';
    }
    if (statusArray.includes('merged')) {
      return '已合并';
    }
    return '关闭';
  }

  loadCommits() {
    const { issueId } = this.props;
    this.setState({ loading: true });
    devOpsApi.loadCommit(issueId)
      .then((res) => {
        this.setState({
          commits: res,
          loading: false,
        });
      });
  }

  createMergeRequest(record) {
    const win = window.open('');
    const { appServiceId, projectId } = record;
    devOpsApi.project(projectId).loadGitUrl(appServiceId)
      .then((res) => {
        const url = `${res}/merge_requests/new?change_branches=true&merge_request[source_branch]=${record.branchName}&merge_request[target_branch]=master`;
        win.location.href = url;
      })
      .catch((error) => {
      });
  }

  handleMenuClick = (record, { key }) => {
    const { issueId } = this.props;
    switch (key) {
      case 'delete': {
        Modal.confirm({
          title: '移除关联分支',
          content: '确定要移除此关联分支吗？',
          okText: '移除',
          onOk: async () => {
            await devOpsApi.project(record.projectId).removeLinkBranch(record.appServiceId, record.branchName, issueId);
            this.loadCommits();
          },
        });
        break;
      }
      case 'merge': {
        this.createMergeRequest(record);
        break;
      }
      default: break;
    }
  }

  render() {
    const column = [
      {
        title: '分支',
        dataIndex: 'branchName',
        width: 200,
        fixed: 'left',
        render: (branchName) => (
          <div style={{ width: '100%', overflow: 'hidden' }}>
            <Tooltip placement="topLeft" mouseEnterDelay={0.5} title={branchName}>
              <p style={{
                overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0,
              }}
              >
                {branchName}
              </p>
            </Tooltip>
          </div>
        ),
      },
      {
        title: '',
        dataIndex: 'id',
        width: 50,
        render: (id, record) => (
          <Dropdown
            overlay={(
              // eslint-disable-next-line react/jsx-no-bind
              <Menu onClick={this.handleMenuClick.bind(this, record)}>
                <Menu.Item key="delete">
                  移除关联分支
                </Menu.Item>
                <Menu.Item key="merge">
                  创建合并请求
                </Menu.Item>
              </Menu>
            )}
            trigger="click"
          >
            <Button shape="circle" icon="more_vert" />
          </Dropdown>
        ),
      },
      {
        title: '应用名称',
        dataIndex: 'appServiceName',
        width: 200,
        render: (appName) => (
          <div style={{ width: '100%', overflow: 'hidden' }}>
            <Tooltip placement="topLeft" mouseEnterDelay={0.5} title={appName}>
              <p style={{
                overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0,
              }}
              >
                {appName}
              </p>
            </Tooltip>
          </div>
        ),
      },
      {
        title: '所属项目',
        dataIndex: 'projectName',
        width: 200,
        render: (projectName) => (
          <div style={{ width: '100%', overflow: 'hidden' }}>
            <Tooltip placement="topLeft" mouseEnterDelay={0.5} title={projectName}>
              <p style={{
                overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0,
              }}
              >
                {projectName}
              </p>
            </Tooltip>
          </div>
        ),
      },
      {
        title: '来源分支',
        dataIndex: 'originBranch',
        width: 200,
        render: (originBranch) => (
          <div style={{ width: '100%', overflow: 'hidden' }}>
            <Tooltip placement="topLeft" mouseEnterDelay={0.5} title={originBranch}>
              <p style={{
                overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0,
              }}
              >
                {originBranch}
              </p>
            </Tooltip>
          </div>
        ),
      },
      {
        title: '提交数',
        dataIndex: 'appId',
        width: 200,
        render: (appId, record) => (
          <div style={{ width: '100%', overflow: 'hidden' }}>
            <Tooltip placement="topLeft" mouseEnterDelay={0.5} title={status}>
              <p style={{
                overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0,
              }}
              >
                {record.commits.length}
              </p>
            </Tooltip>
          </div>
        ),
      },
      {
        title: '状态',
        dataIndex: 'status',
        width: 200,
        render: (status, record) => (
          <div style={{ width: '100%', overflow: 'hidden' }}>
            <Popover
              overlayStyle={{
                boxShadow: '0 5px 5px -3px rgba(0, 0, 0, 0), 0 8px 10px 1px rgba(0, 0, 0, 0), 0 3px 14px 2px rgba(0, 0, 0, 0)',
              }}
              content={(
                <div>
                  {
                    record.mergeRequests && record.mergeRequests.length ? (
                      <ul>
                        {
                          record.mergeRequests.map((v) => (
                            <li style={{ listStyleType: 'none' }}>
                              <span style={{ display: 'inline-block', width: 150 }}>{v.title}</span>
                              <span style={{ display: 'inline-block', width: 50 }}>{['opened', 'merged', 'closed'].includes(v.state) ? STATUS_SHOW[v.state] : ''}</span>
                            </li>
                          ))
                        }
                      </ul>
                    ) : <div>暂无相关合并请求</div>
                  }
                </div>
              )}
            >
              <p style={{
                overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0,
              }}
              >
                {this.getStatus(record.mergeRequests)}
              </p>
            </Popover>
          </div>
        ),
      },
    ];
    return (
      <Table
        pagination={false}
        filterBar={false}
        rowKey={(record) => record.id}
        columns={column}
        dataSource={this.state.commits}
        loading={this.state.loading}
        scroll={{ x: true }}
      />
    );
  }
}
export default Commits;
