/* eslint-disable react/state-in-constructor */
import React, { Component } from 'react';
import { WSHandler, Choerodon } from '@choerodon/boot';
import {
  Progress, Divider,
} from 'choerodon-ui';
import { observer } from 'mobx-react';
import { Modal, Button } from 'choerodon-ui/pro';
import FileSaver from 'file-saver';
import classnames from 'classnames';
import './ImportIssue.less';
import {
  includes, isEqual, uniq, map, isEqualWith, intersection,
} from 'lodash';
import { issueApi } from '@/api';
import { getApplyType, getProjectId } from '@/utils/common';
import ImportFields from './ImportFields';
import TemplateSelect from '../template-select';
import openSaveTemplate from '../template-select/components/save/SaveTemplate';
import SaveTemplateBtn, { transformTemplateJson } from './SaveTemplateBtn';

function customizerFieldCodes(obj1, obj2) {
  if (Array.isArray(obj1) && Array.isArray(obj2)) {
    const newArr = intersection(obj1, obj2);
    return (newArr.length === obj1.length) && (newArr.length === obj2.length);
  }
  return undefined;
}
const ImportIssueForm = (formProps) => {
  const {
    title, children, bottom, className,
  } = formProps;
  return (
    <div className={classnames('c7n-importIssue-form-one', className)}>
      <div style={{
        display: 'flex',
        alignItems: 'center',
      }}
      >
        <span className="c7n-importIssue-form-one-block" />
        <span className="c7n-importIssue-form-one-title">{title}</span>
      </div>
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

    latestInfo: false,
    // eslint-disable-next-line react/no-unused-state
    reRender: false,
    templateIsExist: false,
    templateFirstLoaded: false,
  };

  loadLatestImport = () => {
    issueApi.loadLastImportOrExport(this.props.lastAction || 'upload_file').then((res) => {
      if (res) {
        this.setState({
          latestInfo: res,
        });
      }
    });
  };

  componentDidMount() {
    const { modal } = this.props;
    modal.handleOk(() => this.importExcel());
    this.loadLatestImport();
  }

  componentWillUnmount() {
    // const { historyId, ovn } = this.state;
    // if (historyId) {
    //   issueApi.cancelImport(historyId, ovn);
    // }
    this.finish();
  }

  exportExcel = () => {
    const importFieldsData = { systemFields: [], customFields: [] };
    const allFields = this.importFieldsRef.current?.allFields || [];
    const fields = this.importFieldsRef.current?.fields || [];
    importFieldsData.systemFields = fields.filter((code) => allFields.find((item) => item.code === code && item.system));
    importFieldsData.customFields = fields.filter((code) => allFields.find((item) => item.code === code && !item.system));
    if (this.props.downloadTemplateRequest) {
      this.props.downloadTemplateRequest(importFieldsData).then((excel) => {
        const blob = new Blob([excel], { type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' });
        const fileName = `${this.props.name || '工作项'}导入模板.xlsx`;
        FileSaver.saveAs(blob, fileName);
      });
      return;
    }
    issueApi.downloadTemplateForImport(importFieldsData).then((excel) => {
      const blob = new Blob([excel], { type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' });
      const fileName = `${this.props.name || '工作项'}导入模板.xlsx`;
      FileSaver.saveAs(blob, fileName);
    });
  };

  importExcel = () => {
    this.uploadInput.click();
    return false;
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
    if (this.props.importRequest) {
      this.props.importRequest(formData).then((res) => {
        this.setState({
          uploading: false,
        });
      }).catch((e) => {
        this.setState({
          uploading: false,
        });
        Choerodon.prompt('网络错误');
      }).finally(() => {
        this.uploadInput.value = '';
      });
    } else {
      issueApi.import(formData).then((res) => {
        this.setState({
          uploading: false,
        });
      }).catch((e) => {
        this.setState({
          uploading: false,
        });
        Choerodon.prompt('网络错误');
      }).finally(() => {
        this.uploadInput.value = '';
      });
    }
  };

  handleMessage = (message) => {
    if (message === 'ok') {
      return;
    }
    const { modal } = this.props;
    const data = JSON.parse(message);
    if (data) {
      this.setState({
        wsData: data,
      });
      if (!modal.okProps || !modal.okProps.loading) {
        modal?.update({ okProps: { loading: true } });
      }
      if (data.status === 'success' || data.status === 'template_error_missing_required_column' || data.status === 'template_error' || data.status === 'empty_data_sheet' || data.status?.startsWith('error_custom_field_header')) {
        modal?.update({ okProps: { loading: false } });
        if (this.props.onSuccess && data?.successCount) {
          this.props.onSuccess();
        }
      }
      if (data.status === 'failed') {
        if (data.fileUrl) {
          window.location.href = data.fileUrl;
        }
        modal?.update({ okProps: { loading: false } });
      }
    }
  };

  finish = () => {
    const { onFinish, modal } = this.props;
    if (onFinish) {
      onFinish();
    }
    this.setState({
      wsData: null,
    }, () => modal?.update({ okProps: { loading: false } }));
  };

  renderProgress = () => {
    const { wsData } = this.state;
    const { modal } = this.props;
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
            {`${this.props.name || '工作项'} `}
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
            {`${this.props.name || '工作项'}`}
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
            模板不正确，缺少必要的列
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
      if (isEqualWith(JSON.parse(templateList[i].templateJson), importFieldsData, customizerFieldCodes)) {
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
    openSaveTemplate({ action: this.props.action, onOk: this.templateSelectRef.current?.onOk, fieldCodes: JSON.stringify(importFieldsData) });
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
    const fields = uniq([...(value || []), ...(this.importFieldsRef.current?.requiredFields || [])]).filter((code) => map(allFields, 'code').includes(code));

    importFieldsData.systemFields = fields.filter((code) => allFields.find((item) => item.code === code && item.system)).sort();
    importFieldsData.customFields = fields.filter((code) => allFields.find((item) => item.code === code && !item.system)).sort();
    if (!fields.length) {
      this.setState({
        templateIsExist: true,
      });
      return;
    }
    for (let i = 0; i < templateList.length; i += 1) {
      if (isEqualWith(JSON.parse(templateList[i].templateJson), importFieldsData, customizerFieldCodes)) {
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

  handleInitCheck = (value) => {
    this.handleCheckBoxChangeOk(value);
    this.handleSetTemplateFirstLoaded(false);
  }

  handleSetTemplateFirstLoaded = (loaded) => {
    this.setState({
      templateFirstLoaded: loaded,
    });
  }

  render() {
    const {
      uploading, latestInfo, wsData, templateIsExist, templateFirstLoaded,
    } = this.state;
    const {
      action, requires, systems, fields, messageKey, applyType,
    } = this.props;

    const {
      successCount, failCount, fileUrl, id,
    } = latestInfo;

    const allFields = this.importFieldsRef.current?.allFields || [];
    const requiredFields = this.importFieldsRef.current?.requiredFields || [];

    return (
      <div>
        {
          action && (
            <ImportIssueForm
              title="选择常用模板"
              bottom={null}
              className="c7n-importIssue-templateSelect"
            >
              <TemplateSelect
                templateSelectRef={this.templateSelectRef}
                action={action}
                checkOptions={allFields.map((item) => ({
                  label: item.title,
                  value: item.code,
                  system: item.system,
                  optionConfig: includes(requiredFields, item.code) ? {
                    disabled: includes(requiredFields, item.code),
                    defaultChecked: includes(requiredFields, item.code),
                    name: 'required-option',
                  } : undefined,
                }))}
                selectTemplateOk={this.selectTemplateOk}
                transformExportFieldCodes={(data) => data}
                reverseTransformExportFieldCodes={(data) => data}
                defaultInitCodes={requiredFields}
                setTemplateFirstLoaded={this.handleSetTemplateFirstLoaded}
              />
            </ImportIssueForm>
          )
        }
        <ImportIssueForm
          title="选择模板字段"
          bottom={(
            <>
              <Button
                type="primary"
                onClick={() => this.exportExcel()}
                icon="get_app"
                className="c7n-importIssue-btn"
              >
                下载模板
              </Button>
              {
                action && (
                  <SaveTemplateBtn
                    action={action}
                    importFieldsRef={this.importFieldsRef}
                    templateSelectRef={this.templateSelectRef}
                    checkBoxChangeOk={this.handleInitCheck}
                    templateIsExist={templateIsExist}
                    templateFirstLoaded={templateFirstLoaded}
                  />
                )
              }
            </>
          )}
        >
          {`您必须使用模板文件，录入${this.props.name || '工作项'}信息。`}
          <ImportFields
            importFieldsRef={this.importFieldsRef}
            setReRender={this.handleSetReRender}
            checkBoxChangeOk={this.handleCheckBoxChangeOk}
            requires={requires}
            applyType={applyType}
            systems={systems}
            fields={fields}
          />
        </ImportIssueForm>
        {id && <Divider />}
        {id && (
          <ImportIssueForm
            title={`导入${this.props.name || '工作项'}`}
          >
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

          </ImportIssueForm>
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
          messageKey={messageKey || (getApplyType() === 'program' ? `agile-import-${getProjectId()}` : `agile-import-issues-${getProjectId()}`)}
          onMessage={this.handleMessage}
        >
          {this.renderProgress()}
        </WSHandler>
      </div>
    );
  }
}

const ObserverImportIssue = observer(ImportIssue);

const handleOpenImport = (props) => {
  Modal.open({
    drawer: true,
    className: 'c7n-importIssue',
    maskClosable: false,
    key: Modal.key(),
    title: `导入${props.name || '工作项'}`,
    style: {
      width: 740,
    },
    okText: '导入',
    cancelText: '关闭',
    // footer: (okBtn) => okBtn,
    children: <ObserverImportIssue {...props} applyType="agile" />,
  });
};

export default handleOpenImport;
