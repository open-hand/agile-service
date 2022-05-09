import React, { Component } from 'react';
import { stores, Choerodon } from '@choerodon/boot';
import {
  Modal,
  Table,
} from 'choerodon-ui';
import './Doc.less';
import { produce } from 'immer';
import { knowledgeApi } from '@/api';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import folderIcon from '@/assets/icons/folder.svg';
import documentIcon from '@/assets/icons/document.svg';
import docxIcon from '@/assets/icons/docx.svg';
import pdfIcon from '@/assets/icons/pdf.svg';
import pptxIcon from '@/assets/icons/pptx.svg';
import txtIcon from '@/assets/icons/txt.svg';
import xlsxIcon from '@/assets/icons/xlsx.svg';
import unknownIcon from '@/assets/icons/unknown.svg';

const { AppState } = stores;
const { Sidebar } = Modal;
const FILE_ICON = {
  folder: folderIcon,
  document: documentIcon,
  docx: docxIcon,
  pptx: pptxIcon,
  pdf: pdfIcon,
  txt: txtIcon,
  xlsx: xlsxIcon,
  unknown: unknownIcon,
};

class Doc extends Component {
  constructor(props) {
    super(props);
    this.state = {
      createLoading: false,
      data: [],
      selectedRows: [],
      selectedRowKeys: props.checkIds || [],
      loading: false,
      filter: '',
    };
  }

  componentDidMount() {
    this.loadDoc();
  }

  loadDoc = async () => {
    this.setState({
      loading: true,
    });
    const newData = await knowledgeApi.project(this.props.projectId).loadAllCurrentProject();
    if (newData && !newData.failed) {
      this.setState({
        data: newData,
        loading: false,
      });
    } else {
      this.setState({
        data: [],
        loading: false,
      });
    }
  };

  getColumn = () => [
    {
      title: '知识名称',
      dataIndex: 'name',
      key: 'name',
      render: (value, rowData) => {
        const fileIcon = FILE_ICON[rowData.type === 'file' ? rowData.fileType : (rowData.type || 'folder')] || unknownIcon;
        return (
          <div className="c7n-agile-doc-file-name">
            <img src={fileIcon} className="c7n-agile-doc-file-icon" />
            <span>{value}</span>
          </div>
        );
      },
    },
  ];

  onSelectChange = (selectedRowKeys, selectedRows) => {
    this.setState({
      selectedRows,
      selectedRowKeys,
    });
  };

  getCheckboxProps = (record) => {
    const { checkIds } = this.props;
    return ({
      disabled: checkIds.indexOf(record.id) !== -1,
      name: record.name,
    });
  };

  handleCreateDoc = () => {
    const menu = AppState.currentMenuType;
    const { id: proId } = menu;
    const { selectedRows } = this.state;
    const { issueId, onOk, checkIds } = this.props;
    this.setState({
      createLoading: true,
    });
    if (selectedRows && selectedRows.length) {
      const postData = [];
      selectedRows.forEach((row) => {
        if (checkIds.indexOf(row.id) === -1) {
          postData.push({
            issueId,
            wikiName: row.name,
            wikiUrl: '',
            projectId: proId,
            spaceId: row.id,
          });
        }
      });
      knowledgeApi.project(this.props.projectId).createRelationForIssue(postData).then(() => {
        this.setState({
          createLoading: false,
        });
        onOk();
      }).catch(() => {
        Choerodon.prompt('关联知识文档失败');
        this.setState({
          createLoading: false,
        });
      });
    } else {
      this.setState({
        createLoading: false,
      });
    }
  };

  handleChange=(pagination, filters, sorter, content) => {
    const keyWord = content[0] || '';
    this.setState({
      filter: keyWord,
    });
  }

  getFilteredData() {
    const { data, filter } = this.state;
    return produce(data, (draft) => {
      draft.forEach((base) => {
        // eslint-disable-next-line no-param-reassign
        base.children = base.children.filter((doc) => doc.name.indexOf(filter) + 1);
      });
    }).filter((base) => base.children.length);
  }

  render() {
    const {
      onCancel,
      visible,
    } = this.props;
    const {
      createLoading,
      selectedRowKeys,
      loading,
    } = this.state;
    const { name } = AppState.currentMenuType;

    const rowSelection = {
      selectedRowKeys,
      onChange: this.onSelectChange,
      getCheckboxProps: this.getCheckboxProps,
    };
    return (
      <Sidebar
        maskClosable={false}
        className="c7n-agile-doc"
        title="关联知识"
        visible={visible || false}
        onOk={this.handleCreateDoc}
        onCancel={onCancel}
        okText="确定"
        cancelText="取消"
        confirmLoading={createLoading}
        width={MODAL_WIDTH.middle}
      >
        <div>
          <p>{`你当前项目为"${name}"，知识文档的内容所属为当前项目。`}</p>
          <Table
            dataSource={this.getFilteredData()}
            columns={this.getColumn()}
            rowSelection={rowSelection}
            onChange={this.handleChange}
            // onExpand={this.onExpand}
            rowKey={(record) => record.id}
            pagination={false}
            loading={loading}
            noFilter
          />
        </div>
      </Sidebar>
    );
  }
}
export default Doc;
