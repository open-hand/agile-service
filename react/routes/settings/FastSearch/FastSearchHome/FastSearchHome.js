/* eslint-disable react/jsx-no-bind */
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  Button, Icon,
} from 'choerodon-ui';
import { Modal } from 'choerodon-ui/pro';
import {
  TabPage as Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import CreateFilter from './Component/CreateFilter';
import EditFilter from './Component/EditFilter';
import DeleteFilter from './Component/DeleteFilter';
import FastSearchTable from '../components/table';
import './FastSearchHome.less';

@observer
class Search extends Component {
  constructor(props) {
    super(props);
    this.state = {
      currentFilterId: undefined,
      filter: {},
    };
    const createKey = Modal.key();
    const deleteKey = Modal.key();
    const createFilter = React.createRef();
    const deleteFilter = React.createRef();
    this.tableRef = React.createRef();
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
        className: 'c7nagile-fastSearch-delete',
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

  loadFilters = () => {
    this.tableRef.current?.refresh();
  }

  clickDeleteFilter(record) {
    this.setState({
      filter: record,
      deleteFilterShow: true,
    });
  }

  handleClickMenu = (key, record) => {
    switch (key) {
      case 'delete': {
        this.openDeleteModal(record.toData());
        break;
      }
      default: break;
    }
  }

  handleClickEdit = (record) => {
    this.setState({
      editFilterShow: true,
      currentFilterId: record.get('filterId'),
    });
  }

  render() {
    const {
      editFilterShow,
      deleteFilterShow, filter, currentFilterId,
    } = this.state;

    return (
      <Page
        className="c7n-fast-search"
        service={[
          'choerodon.code.project.setting.issue.ps.fastsearch',
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
          <FastSearchTable
            ref={this.tableRef}
            onEditClick={this.handleClickEdit}
            onMenuClick={this.handleClickMenu}
          />
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
