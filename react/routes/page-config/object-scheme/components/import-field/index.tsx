import React, { useEffect, useRef, useState } from 'react';
import { Modal, Button } from 'choerodon-ui/pro';
import { Choerodon } from '@choerodon/boot';
import { Divider } from 'choerodon-ui';
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
// function useGetWsData(messageKey:string):[any, {}] {
//   const [data, setData] = useState<any>({});

//   const componentProps = {

//   };
//   return [data, componentProps];
// }
const ImportField: React.FC<{ modal?: IModalProps }> = ({ modal }) => {
  const inputRef = useRef<HTMLInputElement>(null);
  const [data, setData] = useState({} as IImportOrExportRecord);
  useEffect(() => {
    pageConfigApi.loadLastImportRecord().then((res) => {
      setData(res);
    });
  }, []);
  function handleDownload() {
    pageConfigApi.downloadTemplate().then((res: any) => {
      const blob = new Blob([res], { type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' });

      fileSaver.saveAs(blob, '字段导入模版.xlsx');
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
    pageConfigApi.import(formData).then(() => {
    }).catch(() => {
      modal?.update({ okProps: { loading: false } });
      Choerodon.prompt('网络错误');
    });
  }
  async function handleImport() {
    inputRef.current?.click();
    return false;
  }
  useEffect(() => {
    modal?.handleOk(handleImport);
  }, [modal]);
  return (
    <div className="im">
      <ImportFormItem
        title="下载模版"
        footer={(
          <Button
            color={'primary' as any}
            onClick={handleDownload}
            icon="get_app"
          >
            下载模板
          </Button>
        )}
      >
        您必须使用模版文件，导入字段信息
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
        messageKey={getMenuType() === 'project' ? `agile-import-customer-field-pro-${getProjectId()}`
          : `agile-import-customer-field-org-${getOrganizationId()}`}
        onStart={(wsData: any) => {
          // setData(wsData);
          modal?.update({ okProps: { loading: true } });
        }}
        onFinish={() => {
          // setData(undefined);
          modal?.update({ okProps: { loading: false } });
        }}
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
function openImportField() {
  /**
     * agile-import-customer-field-org-{organizationId}/agile-import-customer-field-pro-{projectId}
     */
  Modal.open({
    drawer: true,
    className: importStyles.modal,
    maskClosable: false,
    key: Modal.key(),
    title: '导入字段',
    style: {
      width: MODAL_WIDTH.small,
    },
    okText: '导入',
    cancelText: '关闭',
    // footer: (okBtn) => okBtn,
    children: <ImportField />,
  });
}
export default openImportField;
