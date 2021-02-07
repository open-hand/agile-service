import React, { useCallback, useRef, useState } from 'react';
import { Choerodon } from '@choerodon/boot';
import { toJS } from 'mobx';
import { Button, Modal } from 'choerodon-ui/pro';
import { omit } from 'lodash';
import { FuncType, ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { projectReportApi, IProjectReportCreate, IProjectReportUpdate } from '@/api';
import { getProjectId } from '@/utils/common';
import fileSaver from 'file-saver';
import to, { linkUrl } from '@/utils/to';
import styles from './index.less';
import { useProjectReportContext } from '../../context';
import Export, { IExportProps } from '../export';

interface Props {

}
const Operation: React.FC<Props> = () => {
  const {
    store, baseInfoRef, edit, refresh,
  } = useProjectReportContext();
  const [exporting, setExporting] = useState(false);
  const [sending, setSending] = useState(false);
  const exportRef = useRef<IExportProps>({} as IExportProps);
  const handleSubmit = useCallback(async () => {
    const baseInfo = await baseInfoRef.current.submit();
    if (baseInfo && baseInfo instanceof Object) {
      if (edit) {
        const data: IProjectReportUpdate = {
          ...baseInfo,
          objectVersionNumber: store.baseInfo?.objectVersionNumber,
          projectId: getProjectId(),
          reportUnitList: toJS(store.blockList.map((block) => omit(block, 'key'))),
        } as IProjectReportUpdate;
        await projectReportApi.update(store?.baseInfo?.id as string, data);
        refresh();
      } else {
        const data: IProjectReportCreate = {
          ...baseInfo,
          projectId: getProjectId(),
          reportUnitList: toJS(store.blockList.map((block) => omit(block, 'key'))),
        } as IProjectReportCreate;
        await projectReportApi.create(data);
        to('/agile/project-report');
      }
    }
  }, [baseInfoRef, edit, refresh, store.baseInfo?.id, store.baseInfo?.objectVersionNumber, store.blockList]);
  const handlePreview = useCallback(() => {
    window.open(`/#${linkUrl(`/agile/project-report/preview/${store.baseInfo?.id}`, {
      type: 'project',
      params: {
        fullPage: 'true',
      },
    })}`);
  }, [store.baseInfo?.id]);
  const handleExport = useCallback((canvas: HTMLCanvasElement) => {
    setExporting(false);
    canvas.toBlob((blob: Blob) => {
      fileSaver.saveAs(blob, `${store.baseInfo?.title || 'report'}.png`);
    });
  }, [store.baseInfo?.title]);
  const handleExportClick = useCallback(() => {
    setExporting(true);
    exportRef.current?.export(handleExport);
  }, [handleExport]);
  const handleSend = useCallback(async (canvas: HTMLCanvasElement) => {
    if (store.baseInfo?.id) {
      const { objectVersionNumber } = await projectReportApi.send(store.baseInfo?.id, canvas.toDataURL());
      store.setObjectVersionNumber(objectVersionNumber);
      setSending(false);
      Choerodon.prompt('发送成功', 'success');
    }
    setSending(false);
  }, [store]);
  const handleSendClick = useCallback(() => {
    setSending(true);
    exportRef.current?.export(handleSend);
  }, [handleSend]);
  const handleCancelClick = useCallback(() => {
    Modal.confirm({
      title: `确认退出${edit ? '编辑' : '创建'}？`,
      onOk: () => {
        to('/agile/project-report');
      },
    });
  }, [edit]);
  return (
    <div
      className={styles.bar}
    >
      <Button funcType={'raised' as FuncType} color={'blue' as ButtonColor} onClick={handleSubmit}>{edit ? '保存' : '创建'}</Button>
      {edit && (
        <>
          <Button funcType={'raised' as FuncType} onClick={handlePreview}>预览</Button>
          <Button funcType={'raised' as FuncType} onClick={handleExportClick} loading={exporting}>导出</Button>
          <Button
            funcType={'raised' as FuncType}
            loading={sending}
            onClick={handleSendClick}
          >
            发送
          </Button>
        </>
      )}
      <Button funcType={'raised' as FuncType} onClick={handleCancelClick}>取消</Button>
      <Export innerRef={exportRef} />
    </div>
  );
};

export default Operation;
