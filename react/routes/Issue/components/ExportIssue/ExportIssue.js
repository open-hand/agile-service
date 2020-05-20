import React, { Component } from 'react';
import { stores, axios, Choerodon } from '@choerodon/boot';
import { observer } from 'mobx-react';
import { Modal, Radio } from 'choerodon-ui';
import FileSaver from 'file-saver';
import IssueStore from '@/stores/project/issue/IssueStore';
import { find } from 'lodash';

const RadioGroup = Radio.Group;
const { AppState } = stores;
const radioStyle = {
  display: 'block',
  height: '30px',
  lineHeight: '30px',
};
@observer
class ExportIssue extends Component {
  state = {
    mode: 'all',
    loading: false,
  }

  handleExportChange = (e) => {
    this.setState({
      mode: e.target.value,
    });
  }

  handleCancel = () => {
    IssueStore.setExportModalVisible(false);
  }

  getVisibleColumns = () => {
    const fieldTransform = {
      issueNum: 'issueNum',
      issueId: 'summary',
      //  "description":
      issueTypeId: 'typeName',
      //  "projectName":
      assigneeId: 'assigneeName',
      // "assigneeRealName":
      reporterId: 'reporterName',
      //  "reporterRealName":
      //   "resolution":
      statusId: 'statusName',
      issueSprintVOS: 'sprintName',
      // "creationDate":
      lastUpdateDate: 'lastUpdateDate',
      priorityId: 'priorityName',
      //  "subTask":
      //  "remainingTime":
      version: 'versionName',
      epic: 'epicName',
      label: 'labelName',
      storyPoints: 'storyPoints',
      component: 'componentName',
    };
    const { tableRef } = this.props;
    const columns = tableRef.current ? tableRef.current.tableStore.columns.filter(column => column.name && !column.hidden) : [];
    return columns.map(column => fieldTransform[column.name] || column.name);
  };

  /**
   * 输出 excel
   */
  exportExcel = () => {
    const projectId = AppState.currentMenuType.id;
    const orgId = AppState.currentMenuType.organizationId;
    const searchDTO = IssueStore.getCustomFieldFilters();
    const { mode } = this.state;
    const { dataSet } = this.props;
    const field = find([...dataSet.fields.values()], f => f.order);
    const tableShowColumns = mode === 'all' ? [] : this.getVisibleColumns();
    const search = {
      ...searchDTO,
      exportFieldCodes: tableShowColumns,
    };
    this.setState({
      loading: true,
    });
    axios({
      url: `/agile/v1/projects/${projectId}/issues/export`,
      method: 'post',
      data: search,
      params: {
        organizationId: orgId,
        sort: field ? `${field.name},${field.order}` : undefined,
      },
      responseType: 'arraybuffer',
    }).then((data) => {
      const blob = new Blob([data], { type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' });
      const fileName = `${AppState.currentMenuType.name}.xlsx`;
      FileSaver.saveAs(blob, fileName);
      Choerodon.prompt('导出成功');
      IssueStore.setExportModalVisible(false);
    }).finally(() => {
      this.setState({
        loading: false,
      });
    });
  };

  render() {
    const { mode, loading } = this.state;
    const visible = IssueStore.exportModalVisible;
    const projectName = AppState.currentMenuType.name;
    return (
      <Modal
        title="问题列表导出确认"
        visible={visible}
        onOk={this.exportExcel}
        onCancel={this.handleCancel}
        confirmLoading={loading}
      >
        <div style={{ margin: '10px 0' }}>
          您正在导出
          {' '}
          <span style={{ fontWeight: 500 }}>{projectName}</span>
          {' '}
          的问题，请选择你需要导出的字段
        </div>
        <RadioGroup onChange={this.handleExportChange} value={mode}>
          <Radio style={radioStyle} value="show">当前页面显示字段</Radio>
          <Radio style={radioStyle} value="all">全部字段</Radio>
        </RadioGroup>
      </Modal>
    );
  }
}

ExportIssue.propTypes = {

};

export default ExportIssue;
