/* eslint-disable react/state-in-constructor */
import React, { Component } from 'react';
import { WSHandler, Choerodon } from '@choerodon/boot';
import {
  Button, Progress, Divider,
} from 'choerodon-ui';
import { observer } from 'mobx-react';
import { Modal } from 'choerodon-ui/pro';
import FileSaver from 'file-saver';
import './ImportIssue.less';
import { issueApi } from '@/api';
import { getApplyType } from '@/utils/common';
import { includes, isEqual, uniq } from 'lodash';
import ImportFields from './ImportFields';
import TemplateSelect from '../template-select';
import openSaveTemplate from '../template-select/components/save/SaveTemplate';
import SaveTemplateBtn, { transformTemplateJson } from './SaveTemplateBtn';

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
    this.templateSelectRef = React.createRef();
  }

  state = {
    wsData: null,
    historyId: false,
    ovn: false,
    latestInfo: false,
    // eslint-disable-next-line react/no-unused-state
    reRender: false,
    templateIsExist: false,
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

  componentDidMount() {
    this.loadLatestImport();
  }

  componentWillUnmount() {
    const { historyId, ovn } = this.state;
    if (historyId) {
      issueApi.cancelImport(historyId, ovn);
    }
    this.finish();
  }

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
      wsData: null,
      historyId: false,
    });
  };

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

  checkTemplateExist = (value) => {
    const templateList = this.templateSelectRef?.current?.templateList || [];
    const importFieldsData = { systemFields: [], customFields: [] };
    const allFields = this.importFieldsRef.current?.allFields || [];
    const fields = uniq([...(value || []), ...(this.importFieldsRef.current?.requiredFields || [])]);
    importFieldsData.systemFields = fields.filter((code) => allFields.find((item) => item.code === code && item.system)).sort();
    importFieldsData.customFields = fields.filter((code) => allFields.find((item) => item.code === code && !item.system)).sort();

    for (let i = 0; i < templateList.length; i += 1) {
      if (isEqual(transformTemplateJson(templateList[i].templateJson), importFieldsData)) {
        this.setState({
          templateIsExist: true,
        });
        return;
      }
    }
    this.setState({
      templateIsExist: false,
    });
  };

  selectTemplateOk = (fieldCodes) => {
    const newFields = Array.isArray(fieldCodes) ? fieldCodes : [...(fieldCodes.systemFields || []), ...(fieldCodes.customFields || [])];
    this.importFieldsRef.current?.chooseDataSet?.current?.set('fields', [...newFields]);
    this.checkTemplateExist(newFields);
  };

  handleSaveTemplate = () => {
    const importFieldsData = { systemFields: [], customFields: [] };
    const allFields = this.importFieldsRef.current?.allFields || [];
    const fields = this.importFieldsRef.current?.fields || [];
    importFieldsData.systemFields = fields.filter((code) => allFields.find((item) => item.code === code && item.system));
    importFieldsData.customFields = fields.filter((code) => allFields.find((item) => item.code === code && !item.system));
    openSaveTemplate({ action: 'agile_import_issue', onOk: this.templateSelectRef.current?.onOk, fieldCodes: JSON.stringify(importFieldsData) });
  };

  handleSetReRender = () => {
    this.setState({
      // eslint-disable-next-line react/no-unused-state
      reRender: (reRander) => !reRander,
    });
  }

  handleCheckBoxChangeOk = (value) => {
    const templateList = this.templateSelectRef?.current?.templateList || [];
    const importFieldsData = { systemFields: [], customFields: [] };
    const allFields = this.importFieldsRef.current?.allFields || [];
    const fields = uniq([...(value || []), ...(this.importFieldsRef.current?.requiredFields || [])]);
    importFieldsData.systemFields = fields.filter((code) => allFields.find((item) => item.code === code && item.system)).sort();
    importFieldsData.customFields = fields.filter((code) => allFields.find((item) => item.code === code && !item.system)).sort();

    for (let i = 0; i < templateList.length; i += 1) {
      if (isEqual(transformTemplateJson(templateList[i].templateJson), importFieldsData)) {
        this.setState({
          templateIsExist: true,
        });
        this.templateSelectRef?.current?.setTemplate(templateList[i]);
        return;
      }
    }
    this.setState({
      templateIsExist: false,
    });
    this.templateSelectRef?.current?.setTemplate(undefined);
  };

  render() {
    const {
      uploading, latestInfo, wsData, templateIsExist,
    } = this.state;
    const {
      successCount, failCount, fileUrl, id,
    } = latestInfo;

    const allFields = this.importFieldsRef.current?.allFields || [];
    const requiredFields = this.importFieldsRef.current?.requiredFields || [];

    return (
      <div>
        <ImportIssueForm
          title="选择常用模板"
          bottom={null}
        >
          <TemplateSelect
            templateSelectRef={this.templateSelectRef}
            action="agile_import_issue"
            checkOptions={allFields.map((item) => ({
              label: item.title,
              value: item.code,
              disabled: includes(requiredFields, item.code),
              defaultChecked: includes(requiredFields, item.code),
              name: item.title,
            }))}
            selectTemplateOk={this.selectTemplateOk}
            transformExportFieldCodes={(data) => data}
            reverseTransformExportFieldCodes={(data) => data}
          />
        </ImportIssueForm>
        <Divider />
        <ImportIssueForm
          title="选择模板字段"
          bottom={(
            <>
              <Button
                type="primary"
                onClick={() => this.exportExcel()}
                icon="get_app"
              >
                下载模板
              </Button>
              <SaveTemplateBtn
                importFieldsRef={this.importFieldsRef}
                templateSelectRef={this.templateSelectRef}
                checkBoxChangeOk={this.handleCheckBoxChangeOk}
                templateIsExist={templateIsExist}
              />
            </>
          )}
        >
          您必须使用模板文件，录入问题信息。
          <ImportFields importFieldsRef={this.importFieldsRef} setReRender={this.handleSetReRender} checkBoxChangeOk={this.handleCheckBoxChangeOk} />
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
  }
}

const ObserverImportIssue = observer(ImportIssue);

const handleOpenImport = ({ onFinish }) => {
  Modal.open({
    drawer: true,
    className: 'c7n-importIssue',
    maskClosable: false,
    key: Modal.key(),
    title: '导入问题',
    style: {
      width: 380,
    },
    okText: '关闭',
    footer: (okBtn) => okBtn,
    children: <ObserverImportIssue onFinish={onFinish} />,
  });
};

export default handleOpenImport;
