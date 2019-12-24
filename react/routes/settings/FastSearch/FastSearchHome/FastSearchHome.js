import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Link } from 'react-router-dom';
import {
  Button, Spin, Tooltip, Icon, Menu, Breadcrumb as Bread,
} from 'choerodon-ui';
import { Modal } from 'choerodon-ui/pro';
import {
  TabPage as Page, Header, Content, stores, axios, Breadcrumb,
} from '@choerodon/boot';
import CreateFilter from './Component/CreateFilter';
import EditFilter from './Component/EditFilter';
import DeleteFilter from './Component/DeleteFilter';
import SortTable from './Component/SortTable';
import TableDropMenu from '../../../../common/TableDropMenu';
import './FastSearchHome.less';

const { AppState } = stores;
const { Item } = Bread;

@observer
class Search extends Component {
  constructor(props) {
    super(props);
    this.state = {
      filters: [],
      currentFilterId: undefined,
      filter: {},
      loading: false,
      barFilters: [],
      filterName: '',
    };
    const createKey = Modal.key();
    const editKey = Modal.key();
    const deleteKey = Modal.key();
    const createFilter = React.createRef();
    const editFilter = React.createRef();
    const deleteFilter = React.createRef();
    this.openCreateModal = () => {
      Modal.open({
        key: createKey,
        title: '创建快速筛选',
        style: {
          width: 740,
        },
        className: 'c7nagile-fastSearch-create',
        drawer: true,
        okText: '创建',
        cancelText: '取消',
        children: (
          <CreateFilter
            forwardref={createFilter}
            onOk={() => {
              this.loadFilters();
            }}
          />
        ),
        onOk: () => createFilter.current.handleSubmit(),
      });
    };

    this.openDeleteModal = (filter) => {
      Modal.open({
        key: deleteKey,
        title: '删除快速筛选',
        style: {
          width: 520,
        },
        okText: '删除',
        okType: 'danger',
        cancelText: '取消',
        children: (
          <DeleteFilter
            forwardref={deleteFilter}
            filter={filter}
            onOk={() => {
              this.loadFilters();
            }}
          />
        ),
        onOk: () => { deleteFilter.current.handleDelete(); },
      });
    };
  }

  componentDidMount() {
    this.loadFilters();
  }

  transformOperation = (str) => {
    // 注意该对象key的顺序
    const OPERATION = {
      '!=': '不等于',
      'not in': '不包含',
      in: '包含',
      'is not': '不是',
      is: '是',
      '<=': '小于或等于',
      '<': '小于',
      '>=': '大于或等于',
      '>': '大于',
      '=': '等于',
      OR: '或',
      AND: '与',
    };

    let transformKey = str;
    Object.keys(OPERATION).forEach((v) => {
      transformKey = transformKey.replace(new RegExp(` ${v} `, 'g'), ` ${OPERATION[v]} `);
    });
    return transformKey;
  };

  handleDrag = (data, postData) => {
    this.setState({
      filters: data,
    });
    axios
      .put(`/agile/v1/projects/${AppState.currentMenuType.id}/quick_filter/drag`, postData)
      .then(() => {
        axios.post(`/agile/v1/projects/${AppState.currentMenuType.id}/quick_filter/query_all`, {
          contents: [
          ],
          filterName: '',
        }).then((res) => {
          this.setState({
            filters: res,
          });
        });
      })
      .catch(() => {
        axios.post(`/agile/v1/projects/${AppState.currentMenuType.id}/quick_filter/query_all`, {
          contents: [
          ],
          filterName: '',
        }).then((ress) => {
          this.setState({
            filters: ress,
          });
        });
      });
  };

  handleTableChange = (pagination, filters, sorter, barFilters) => {
    this.setState({
      filterName: filters.name && filters.name[0],
      barFilters,
    }, () => {
      this.loadFilters();
    });
  }

  clickDeleteFilter(record) {
    this.setState({
      filter: record,
      deleteFilterShow: true,
    });
  }

  deleteComponent() {
    this.loadComponents();
  }

  loadFilters(page = 0, size = 10) {
    const { filterName, barFilters } = this.state;
    this.setState({
      loading: true,
    });
    axios.post(`/agile/v1/projects/${AppState.currentMenuType.id}/quick_filter/query_all`, {
      contents: barFilters,
      filterName,
    })
      .then((res) => {
        this.setState({
          filters: res,
          loading: false,
        });
      })
      .catch((error) => { });
  }


  handleClickMenu = (record, { key }) => {
    switch (key) {
      case 'editFilter': {
        this.setState({
          editFilterShow: true,
          currentFilterId: record.filterId,
        });
        break;
      }
      case 'deleteFilter': {
        this.openDeleteModal(record);
        break;
      }
      default: break;
    }
  }

  handleClickEdit = (record) => {
    this.setState({
      editFilterShow: true,
      currentFilterId: record.filterId,
    });
  }

  renderMenu = record => (
    <Menu onClick={this.handleClickMenu.bind(this, record)}>
      <Menu.Item key="deleteFilter">
        <span>删除</span>
      </Menu.Item>
    </Menu>
  )

  render() {
    const {
      loading, filters, editFilterShow,
      deleteFilterShow, filter, currentFilterId,
    } = this.state;

    const menu = AppState.currentMenuType;
    const {
      type, id, organizationId: orgId, name, 
    } = menu;

    const column = [
      {
        title: '名称',
        dataIndex: 'name',
        width: '3.5rem',
        render: (text, record) => (
          <TableDropMenu
            menu={this.renderMenu(record)}
            onClickEdit={this.handleClickEdit.bind(this, record)}
            text={(
              <Tooltip placement="topLeft" mouseEnterDelay={0.5} title={name}>
                <p
                  style={{
                    width: '3.5rem',
                    overflow: 'hidden',
                    textOverflow: 'ellipsis',
                    whiteSpace: 'nowrap',
                    marginBottom: 0,
                  }}
                >
                  {text}
                </p>
              </Tooltip>
            )}
          />
        ),
        filters: [],
      },
      {
        title: '筛选器',
        dataIndex: 'expressQuery',
        width: '50%',
        render: expressQuery => (
          <div style={{
            maxWidth: '422px',
            overflow: 'hidden',
            textOverflow: 'ellipsis',
            whiteSpace: 'nowrap',
          }}
          >
            <Tooltip placement="topLeft" mouseEnterDelay={0.5} title={this.transformOperation(expressQuery)}>
              <span
                style={{
                  overflow: 'hidden',
                  textOverflow: 'ellipsis',
                  whiteSpace: 'nowrap',
                  marginBottom: 0,
                  lineHeight: '24px',
                }}
              >
                {/* {expressQuery} */}
                {this.transformOperation(expressQuery)}
              </span>
            </Tooltip>
          </div>
        ),
      },
      {
        title: '描述',
        dataIndex: 'description',
        width: '25%',
        render: description => (
          <div style={{
            maxWidth: '288px',
            overflow: 'hidden',
            textOverflow: 'ellipsis',
            whiteSpace: 'nowrap',
          }}
          >
            <Tooltip placement="topLeft" mouseEnterDelay={0.5} title={description.split('+++')[0]}>
              <span
                style={{
                  overflow: 'hidden',
                  textOverflow: 'ellipsis',
                  whiteSpace: 'nowrap',
                  marginBottom: 0,
                  lineHeight: '24px',
                }}
              >
                {description.split('+++')[0] || ''}
              </span>
            </Tooltip>
          </div>
        ),
      },
    ];

    return (
      <Page 
        className="c7n-fast-search"
        service={[
          'agile-service.quick-filter.listByProjectId',
          'agile-service.quick-filter.queryById',
          'agile-service.quick-filter.update',
          'agile-service.quick-filter.create',
          'agile-service.quick-filter.checkName',
          'agile-service.quick-filter.dragFilter',
          'agile-service.quick-filter.list',
          'agile-service.quick-filter.deleteById',
        ]}
      >
        <Header title="快速筛选">
          <Button funcType="flat" onClick={this.openCreateModal}>
            <Icon type="playlist_add icon" />
            <span>创建快速筛选</span>
          </Button>
        </Header>
        <Breadcrumb />
        <Content>
          <div>
            <Spin spinning={loading}>
              <SortTable
                onChange={this.handleTableChange}
                handleDrag={this.handleDrag}
                rowKey={record => record.filterId}
                columns={column}
                dataSource={filters}
                scroll={{ x: true }}
              />
            </Spin>
          </div>
          {editFilterShow ? (
            <EditFilter
              filterId={currentFilterId}
              onOk={() => {
                this.setState({ editFilterShow: false });
                this.loadFilters();
              }}
              onCancel={() => this.setState({ editFilterShow: false })}
            />
          ) : null}
          {deleteFilterShow ? (
            <DeleteFilter
              filter={filter}
              onOk={() => {
                this.setState({ deleteFilterShow: false });
                this.loadFilters();
              }}
              onCancel={() => this.setState({ deleteFilterShow: false })}
            />
          ) : null}
        </Content>
      </Page>
    );
  }
}

export default Search;
