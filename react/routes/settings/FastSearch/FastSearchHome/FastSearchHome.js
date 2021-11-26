/* eslint-disable react/jsx-no-bind */
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Modal } from 'choerodon-ui/pro';
import { HeaderButtons, C7NFormat } from '@choerodon/master';

import {
  TabPage as Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import DeleteFilter from './Component/DeleteFilter';
import FastSearchTable from '../components/table';
import './FastSearchHome.less';
import { openCreateFastSearch, openEditFastSearch } from '../components/create-edit-fast-search';

@observer
class Search extends Component {
  constructor(props) {
    super(props);
    this.state = {
      filter: {},
    };
    const deleteKey = Modal.key();
    const deleteFilter = React.createRef();
    this.tableRef = React.createRef();
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
    openEditFastSearch(record.get('filterId'), () => {
      this.loadFilters();
    });
  }

  render() {
    const { deleteFilterShow, filter } = this.state;

    return (
      <Page
        className="c7n-fast-search"
        service={[
          'choerodon.code.project.setting.issue.ps.fastsearch',
        ]}
      >
        <Header title="快速筛选">
          <HeaderButtons items={[{
            name: (
              <span>
                <C7NFormat
                  intlPrefix="agile.setting"
                  id="create.filter"
                />
              </span>),
            icon: 'playlist_add',
            handler: () => openCreateFastSearch(() => this.loadFilters()),
            display: true,
          }]}
          />
        </Header>
        <Breadcrumb />
        <Content>
          <FastSearchTable
            ref={this.tableRef}
            onEditClick={this.handleClickEdit}
            onMenuClick={this.handleClickMenu}
          />
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
