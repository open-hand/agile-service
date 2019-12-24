import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Link } from 'react-router-dom';
import {
  Button, Table, Spin, Tooltip, Icon, Menu, Breadcrumb as Bread,
} from 'choerodon-ui';
import {
  TabPage as Page, Header, Content, stores, axios, Permission, Breadcrumb,
} from '@choerodon/boot';
import CreateLink from './Component/CreateLink';
import EditLink from './Component/EditLink';
import DeleteLink from './Component/DeleteLink';
import './IssueLinkHome.less';
import TableDropMenu from '../../../../common/TableDropMenu';

const { AppState } = stores;
const { Item } = Bread;

@observer
class IssueLink extends Component {
  constructor(props) {
    super(props);
    this.state = {
      links: [],
      createLinkShow: false,
      editLinkShow: false,
      currentLinkTypeId: undefined,
      link: {},
      loading: false,
      confirmShow: false,
      pagination: {
        current: 1,
        pageSize: 10,
        total: undefined,
      },
      filterName: '',
      barFilters: [],
    };
  }

  componentDidMount() {
    const { filterName, barFilters, pagination } = this.state;
    this.loadLinks(pagination.current, pagination.pageSize);
  }

  handleTableChange = (pagination, filters, sorter, barFilters) => {
    this.setState({
      pagination,
      filterName: filters.linkName && filters.linkName[0],
      barFilters,
    }, () => {
      this.loadLinks(pagination.current, pagination.pageSize);
    });
  }

  showLinkType(record) {
    this.setState({
      editLinkShow: true,
      currentLinkTypeId: record.linkTypeId,
    });
  }

  clickDeleteLink(record) {
    this.setState({
      link: record,
      confirmShow: true,
    });
  }

  deleteLink() {
    this.setState({
      confirmShow: false,
    });
    this.loadLinks();
  }

  loadLinks(page = 1, size = 10) {
    const { filterName, barFilters } = this.state;
    this.setState({
      loading: true,
    });
    axios
      .post(`/agile/v1/projects/${AppState.currentMenuType.id}/issue_link_types/query_all?page=${page}&size=${size}`, {
        contents: barFilters,
        linkName: filterName,
      })
      .then((res) => {
        this.setState({
          links: res.list,
          loading: false,
          pagination: {
            current: res.pageNum,
            pageSize: res.pageSize,
            total: res.total,
          },
        });
      })
      .catch((error) => { });
  }


  render() {
    const menu = AppState.currentMenuType;
    const {
      type, id, organizationId: orgId, name,
    } = menu;
    const {
      loading, links, pagination, createLinkShow, editLinkShow, currentLinkTypeId, confirmShow, link, filterName, barFilters,
    } = this.state;

    const column = [
      {
        title: '名称',
        dataIndex: 'linkName',
        width: '3rem',
        render: (linkName, record) => {
          const menus = (
            <Menu onClick={this.clickDeleteLink.bind(this, record)}>
              <Menu.Item key="del">
                <Permission
                  type={type}
                  projectId={id}
                  organizationId={orgId}
                  service={['agile-service.issue-link-type.deleteIssueLinkType']}
                >
                  <span>删除</span>
                </Permission>
              </Menu.Item>
            </Menu>
          );
          return (
            <TableDropMenu
              menu={menus}
              onClickEdit={this.showLinkType.bind(this, record)}
              text={(
                <Tooltip placement="topLeft" mouseEnterDelay={0.5} title={linkName}>
                  <p
                    style={{
                      width: '3rem',
                      overflow: 'hidden',
                      textOverflow: 'ellipsis',
                      whiteSpace: 'nowrap',
                      marginBottom: 0,
                    }}
                  >
                    <Permission
                      type={type}
                      projectId={id}
                      organizationId={orgId}
                      service={['agile-service.issue-link-type.updateIssueLinkType']}
                    >
                      {linkName}
                    </Permission>
                  </p>
                </Tooltip>
              )}
            />
          );
        },
        filters: [],
      },
      {
        title: '链出描述',
        dataIndex: 'outWard',
        width: '30%',
        render: outWard => (
          <div style={{ width: '100%', overflow: 'hidden' }}>
            <Tooltip placement="topLeft" mouseEnterDelay={0.5} title={outWard}>
              <p
                style={{
                  overflow: 'hidden',
                  textOverflow: 'ellipsis',
                  whiteSpace: 'nowrap',
                  marginBottom: 0,
                  lineHeight: '24px',
                }}
              >
                {outWard}
              </p>
            </Tooltip>
          </div>
        ),
      },
      {
        title: '链入描述',
        dataIndex: 'inWard',
        width: '30%',
        render: inWard => (
          <div style={{ width: '100%', overflow: 'hidden' }}>
            <Tooltip placement="topLeft" mouseEnterDelay={0.5} title={inWard}>
              <p
                style={{
                  overflow: 'hidden',
                  textOverflow: 'ellipsis',
                  whiteSpace: 'nowrap',
                  marginBottom: 0,
                  lineHeight: '24px',
                }}
              >
                {inWard}
              </p>
            </Tooltip>
          </div>
        ),
      },
    ];
    return (
      <Page
        service={[
          'agile-service.issue-link-type.listIssueLinkType',
          'agile-service.issue-link-type.updateIssueLinkType',
          'agile-service.issue-link-type.deleteIssueLinkType',
          'agile-service.issue-link-type.createIssueLinkType',
        ]}
        className="c7n-issue-link"
      >
        <Header title="问题链接">
          <Permission
            type={type}
            projectId={id}
            organizationId={orgId}
            service={['agile-service.issue-link-type.createIssueLinkType']}
          >
            <Button funcType="flat" onClick={() => this.setState({ createLinkShow: true })}>
              <Icon type="playlist_add icon" />
              <span>创建链接</span>
            </Button>
          </Permission>
        </Header>
        <Breadcrumb />
        <Content>
          <div>
            <Spin spinning={loading}>
              <Table
                pagination={pagination}
                rowKey={record => record.linkTypeId}
                columns={column}
                dataSource={links}
                filterBarPlaceholder="过滤表"
                scroll={{ x: true }}
                onChange={this.handleTableChange}
              />
            </Spin>
            {createLinkShow ? (
              <CreateLink
                onOk={() => {
                  this.setState({ createLinkShow: false });
                  this.loadLinks();
                }}
                onCancel={() => this.setState({ createLinkShow: false })}
              />
            ) : null}
            {editLinkShow ? (
              <EditLink
                linkTypeId={currentLinkTypeId}
                onOk={() => {
                  this.setState({ editLinkShow: false });
                  this.loadLinks();
                }}
                onCancel={() => this.setState({ editLinkShow: false })}
              />
            ) : null}
            {confirmShow ? (
              <DeleteLink
                visible={confirmShow}
                link={link}
                onCancel={() => this.setState({ confirmShow: false })}
                onOk={this.deleteLink.bind(this)}
              />
            ) : null}
          </div>
        </Content>
      </Page>
    );
  }
}

export default IssueLink;
