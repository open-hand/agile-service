import React, {
  useCallback, useEffect, useRef, useState,
} from 'react';
import { Modal, Button } from 'choerodon-ui/pro';
import { Choerodon } from '@choerodon/boot';
import { C7NFormat } from '@choerodon/master';
import { Divider } from 'choerodon-ui';
import { set } from 'lodash';
import fileSaver from 'file-saver';
import WsProgress from '@/components/ws-progress';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { getMenuType, getOrganizationId, getProjectId } from '@/utils/common';
import { IImportOrExportRecord, pageConfigApi } from '@/api';
import { IModalProps } from '@/common/types';
import importStyles from './index.less';

interface IImportFormItemProps {
  title: React.ReactNode
  footer?: React.ReactNode
}
const ImportFormItem: React.FC<IImportFormItemProps> = ({ title, footer, children }) => (
  <div className={importStyles.form_item}>
    <span className={importStyles.form_item_title}>{title}</span>
    <span className={importStyles.form_item_content}>{children}</span>
    {footer}
  </div>
);

const ImportField: React.FC<{ modal?: IModalProps, onOk: Function }> = ({ modal, onOk }) => {
  const inputRef = useRef<HTMLInputElement>(null);
  const [data, setData] = useState({} as IImportOrExportRecord);
  const [wsData, setWsData] = useState({} as IImportOrExportRecord);
  function loadRecord() {
    pageConfigApi.loadLastImportRecord().then((res) => {
      setData(res);
    });
  }
  useEffect(() => {
    loadRecord();
  }, []);
  function handleDownload() {
    pageConfigApi.downloadTemplate().then((res: any) => {
      const blob = new Blob([res], { type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' });

      fileSaver.saveAs(blob, '字段导入模板.xlsx');
    });
  }

  function handleUpload(file: File | null) {
    if (!file) {
      Choerodon.prompt('请选择文件');
      return;
    }
    const formData = new FormData();
    formData.append('file', file);
    modal?.update({ okProps: { loading: true } });

    pageConfigApi.import(formData).catch(() => {
      Choerodon.prompt('网络错误');
      modal?.update({ okProps: { loading: false } });
    }).finally(() => {
      set(inputRef.current || {}, 'value', '');
    });
  }
  const handleFinish = useCallback((newWsData?: any) => {
    loadRecord();
    setWsData(() => {
      modal?.update({ okText: '导入' });
      if (newWsData.status !== 'failed') {
        onOk();
        modal?.close();
      }
      return newWsData;
    });
  }, [modal]);
  const handleImport = useCallback(async () => {
    if (wsData.status !== 'doing') {
      inputRef.current?.click();
    } else if (wsData.id && wsData.objectVersionNumber) {
      pageConfigApi.importCancel({ id: wsData.id, objectVersionNumber: wsData.objectVersionNumber });
      handleFinish({});
    }
    return false;
  }, [handleFinish, wsData]);
  useEffect(() => {
    modal?.handleOk(handleImport);
  }, [handleImport, modal]);
  return (
    <div className="im">
      <ImportFormItem
        title="下载模板"
        footer={(
          <Button
            onClick={handleDownload}
            icon="get_app"
          >
            下载模板
          </Button>
        )}
      >
        您必须使用模板文件，导入字段信息
      </ImportFormItem>
      {data.id && <Divider />}
      {data.id && (
        <ImportFormItem title="导入字段信息">
          {/* 上次导入时间 */}
          <div style={{ marginTop: 10 }}>
            上次导入共导入
            <span style={{ color: '#00bfa5', fontSize: 20, margin: '0 .04rem' }}>{data.successCount}</span>
            条数据成功,
            <span style={{ color: '#f76e64', fontSize: 20, margin: '0 .04rem' }}>{data.failCount}</span>
            条数据失败
            {data.fileUrl && (
              <a href={data.fileUrl} style={{ display: 'inline-block', marginTop: '.04rem' }}>
                点击下载失败详情
              </a>
            )}
          </div>

        </ImportFormItem>
      )}
      <WsProgress
        className={importStyles.progress}
        visible={wsData.status === 'doing'}
        messageKey={getMenuType() === 'project' ? `agile-import-customer-field-pro-${getProjectId()}`
          : `agile-import-customer-field-org-${getOrganizationId()}`}
        onStart={(newWsData: any) => {
          setWsData(() => {
            modal?.update({ okProps: { loading: false }, okText: '取消导入' });
            return newWsData;
          });
        }}
        handleMessage={(newWsData: any) => {
          if (newWsData.status === 'failed') {
            return true;
          }
          return false;
        }}
        autoDownload
        onFinish={handleFinish}
        predefineProgressTextConfig="import"
      />
      <input
        ref={inputRef}
        type="file"
        onChange={(e) => {
          if (e.target.files && e.target.files[0]) {
            handleUpload(e.target.files[0]);
          }
        }}
        style={{ display: 'none' }}
        accept="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
      />
    </div>
  );
};
function openImportField({ onOk }: { onOk: Function }) {
  /**
     * agile-import-customer-field-org-{organizationId}/agile-import-customer-field-pro-{projectId}
     */
  Modal.open({
    drawer: true,
    className: importStyles.modal,
    maskClosable: false,
    key: Modal.key(),
    title: <C7NFormat
      intlPrefix="agile.page"
      id="field.import"
    />,
    style: {
      width: MODAL_WIDTH.small,
    },
    okText: '导入',
    cancelText: '关闭',
    // footer: (okBtn) => okBtn,
    children: <ImportField onOk={onOk} />,
  });
}
export default openImportField;
