/* eslint-disable react/state-in-constructor */
import React, { Component } from 'react';
import { WSHandler, Choerodon } from '@choerodon/boot';
import {
  Modal, Button, Progress, Divider,
} from 'choerodon-ui';
import { observer } from 'mobx-react';
import { Button as ButtonPro } from 'choerodon-ui/pro';
import FileSaver from 'file-saver';
import './ImportIssue.less';
import { issueApi } from '@/api';
import { getApplyType } from '@/utils/common';
import ImportFields from './ImportFields';

const ImportIssueForm = (formProps) => {
  const { title, children, bottom } = formProps;
  return (
    <div className="c7n-importIssue-form-one">
      <span className="c7n-importIssue-form-one-title">{title}</span>
      <span className="c7n-importIssue-form-one-content">{children}</span>
      {bottom}
    </div>
  );
};
class ImportIssue extends Component {
  constructor(props) {
    super(props);
    this.importFieldsRef = React.createRef();
  }

  state = {
    visible: false,
    wsData: null,
    historyId: false,
    ovn: false,
    latestInfo: false,
  };

  loadLatestImport = () => {
    issueApi.loadLastImportOrExport('upload_file').then((res) => {
      if (res) {
        this.setState({
          latestInfo: res,
          historyId: res.status === 'doing' ? res.id : false,
          ovn: res.objectVersionNumber,
        });
      }
    });
  };

  open = () => {
    this.setState({
      visible: true,
    });
    this.loadLatestImport();
  };

  onCancel = () => {
    const { historyId, ovn } = this.state;
    if (historyId) {
      issueApi.cancelImport(historyId, ovn);
    }
    this.finish();
  };

  exportExcel = () => {
    const importFieldsData = { systemFields: [], customFields: [] };
    const allFields = this.importFieldsRef.current?.allFields || [];
    const fields = this.importFieldsRef.current?.fields || [];
    importFieldsData.systemFields = fields.filter((code) => allFields.find((item) => item.code === code && item.system));
    importFieldsData.customFields = fields.filter((code) => allFields.find((item) => item.code === code && !item.system));
    issueApi.downloadTemplateForImport(importFieldsData).then((excel) => {
      const blob = new Blob([excel], { type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' });
      const fileName = '问题导入模板.xlsx';
      FileSaver.saveAs(blob, fileName);
    });
  };

  importExcel = () => {
    this.uploadInput.click();
  };

  beforeUpload = (e) => {
    if (e.target.files[0]) {
      this.upload(e.target.files[0]);
    }
  };

  upload = (file) => {
    if (!file) {
      Choerodon.prompt('请选择文件');
      return;
    }
    const formData = new FormData();
    formData.append('file', file);
    this.setState({
      uploading: true,
    });
    issueApi.import(formData).then((res) => {
      this.setState({
        uploading: false,
      });
    }).catch((e) => {
      this.setState({
        uploading: false,
      });
      Choerodon.prompt('网络错误');
    });
  };

  handleMessage = (message) => {
    if (message === 'ok') {
      return;
    }
    const data = JSON.parse(message);
    if (data) {
      this.setState({
        wsData: data,
        historyId: data.id,
        ovn: data.objectVersionNumber,
      });
      if (data.status === 'failed') {
        if (data.fileUrl) {
          window.location.href = data.fileUrl;
        }
      }
    }
  };

  finish = () => {
    const { onFinish } = this.props;
    if (onFinish) {
      onFinish();
    }
    this.setState({
      visible: false,
      wsData: null,
      historyId: false,
    });
  };

  renderFooter = () => <ButtonPro color="primary" funcType="raised" onClick={this.finish}>关闭</ButtonPro>;

  renderProgress = () => {
    const { wsData } = this.state;
    if (!wsData) {
      return null;
    }
    const {
      process = 0,
      status,
      failCount,
      fileUrl,
      successCount,
    } = wsData;

    if (status === 'doing') {
      return (
        <div className="c7n-importIssue-progress-area">
          <Progress
            className="c7n-importIssue-progress"
            status="active"
            type="circle"
            width={50}
            percent={(process * 100).toFixed(0)}
            strokeWidth={16}
            showInfo={false}
          />
          <span className="c7n-importIssue-progress-area-text">正在导入中</span>
          <span className="c7n-importIssue-progress-area-prompt">( 本次导入耗时较长，您可先返回进行其他操作）</span>
        </div>
      );
    } if (status === 'failed') {
      return (
        <div>
          <span className="c7n-importIssue-text">
            导入失败
            <span style={{ color: '#FF0000' }}>{failCount}</span>
            问题
            <a href={fileUrl}>
              点击下载失败详情
            </a>
          </span>
        </div>
      );
    } if (status === 'success') {
      return (
        <div>
          <span className="c7n-importIssue-text">
            导入成功
            <span style={{ color: '#0000FF' }}>{successCount}</span>
            问题
          </span>
        </div>
      );
    } if (status === 'template_error') {
      return (
        <div>
          <span className="c7n-importIssue-text">
            导入模板错误，或无数据。
          </span>
        </div>
      );
    }
    if (status === 'empty_data_sheet') {
      return (
        <div>
          <span className="c7n-importIssue-text">
            导入数据为空
          </span>
        </div>
      );
    }
    if (status === 'template_error_missing_required_column') {
      return (
        <div>
          <span className="c7n-importIssue-text">
            模版不正确，缺少必要的列
          </span>
        </div>
      );
    }
    if (status.startsWith('error_custom_field_header')) {
      const msg = status.split('error_custom_field_header_')[1];
      return (
        <div>
          <span className="c7n-importIssue-text">
            {`自定义字段${msg}不存在`}
          </span>
        </div>
      );
    }
    return (
      <div>
        正在查询导入信息，请稍后
      </div>
    );
  };

  renderForm = () => {
    const { uploading, latestInfo, wsData } = this.state;
    const {
      successCount, failCount, fileUrl, id,
    } = latestInfo;
    return (
      <div>
        <ImportIssueForm
          title="下载模板"
          bottom={(
            <Button
              type="primary"
              onClick={() => this.exportExcel()}
              icon="get_app"
            >
              下载模板
            </Button>
          )}
        >
          您必须使用模板文件，录入问题信息。
          <ImportFields importFieldsRef={this.importFieldsRef} />
        </ImportIssueForm>
        <Divider />
        <ImportIssueForm
          title="导入问题"
          bottom={!wsData && (
            <Button
              loading={uploading}
              type="primary"
              onClick={() => this.importExcel()}
              icon="archive"
            >
              导入问题
            </Button>
          )}
        >
          {id && (
            <div style={{ marginTop: 10 }}>
              上次导入共导入
              <span style={{ color: '#00bfa5', fontSize: 20, margin: '0 .04rem' }}>{successCount}</span>
              条数据成功,
              <span style={{ color: '#f76e64', fontSize: 20, margin: '0 .04rem' }}>{failCount}</span>
              条数据失败
              {fileUrl && (
                <a href={fileUrl}>
                  点击下载失败详情
                </a>
              )}
            </div>
          )}
          <input
            ref={
              (uploadInput) => { this.uploadInput = uploadInput; }
            }
            type="file"
            onChange={this.beforeUpload}
            style={{ display: 'none' }}
            accept="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
          />
          <WSHandler
            messageKey={getApplyType() === 'program' ? 'agile-import' : 'agile-import-issues'}
            onMessage={this.handleMessage}
          >
            {this.renderProgress()}
          </WSHandler>
        </ImportIssueForm>
      </div>
    );
  };

  render() {
    const {
      visible,
    } = this.state;
    return (
      <Modal.Sidebar
        className="c7n-importIssue"
        title="导入问题"
        width={380}
        visible={visible}
        onCancel={this.onCancel}
        footer={this.renderFooter()}
        destroyOnClose
      >
        {this.renderForm()}
      </Modal.Sidebar>
    );
  }
}

export default observer(ImportIssue);
