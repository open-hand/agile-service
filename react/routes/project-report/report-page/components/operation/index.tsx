import React, {
  useCallback, useEffect, useRef, useState,
} from 'react';
import { Choerodon } from '@choerodon/boot';
import { toJS } from 'mobx';
import { Button, Modal } from 'choerodon-ui/pro';
import { omit } from 'lodash';
import { FuncType, ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import {
  projectReportApi, IProjectReportCreate, IProjectReportUpdate, fileApi,
} from '@/api';
import { getProjectId } from '@/utils/common';
// import JsPDF from 'jspdf';
import fileSaver from 'file-saver';
import to, { linkUrl } from '@/utils/to';
import {
  Box, Email, Image, Item, renderEmail,
} from 'react-html-email';
import uuidV4 from 'uuid/v4';
import styles from './index.less';
import { useProjectReportContext } from '../../context';
import Export, { IExportProps } from '../export';

const htmlTemplate = (images: { url: String, height: number, width?: number | string }[]) => renderEmail(
  <Box width="100%">
    {images.map((image) => (
      <Item>
        <Image
          src={image.url}
          style={{
            maxWidth: image.width || 'calc(100% - 310px)',
            margin: '0 auto',
          }}
        />
      </Item>
    ))}
  </Box>
  ,
);

interface Props {

}
const Operation: React.FC<Props> = () => {
  const {
    store, baseInfoRef, edit, refresh,
  } = useProjectReportContext();
  const [exporting, setExporting] = useState(false);
  const [sending, setSending] = useState(false);
  const [dataKeySet] = useState(new Set());
  const exportRef = useRef<IExportProps>({} as IExportProps);

  useEffect(() => clearDataKey, []);

  const clearDataKey = useCallback(() => {
    dataKeySet.forEach((value) => {
      // @ts-ignore
      window[value] && (window[value] = undefined);
    });
    dataKeySet.clear();
  }, [dataKeySet]);

  const getPostData = useCallback(async () => {
    const baseInfo = await baseInfoRef.current.submit();
    if (baseInfo && baseInfo instanceof Object) {
      const data: IProjectReportCreate = {
        ...baseInfo,
        projectId: getProjectId(),
        reportUnitList: toJS(store.blockList.map((block) => omit(block, 'key'))),
      } as IProjectReportCreate;
      return data;
    }
    return null;
  }, [baseInfoRef, store]);
  const handleSubmit = useCallback(async () => {
    const postData = await getPostData();
    if (postData) {
      if (edit) {
        const data: IProjectReportUpdate = {
          ...postData,
          objectVersionNumber: store.baseInfo?.objectVersionNumber,
        } as IProjectReportUpdate;
        await projectReportApi.update(store?.baseInfo?.id as string, data);
        store.dirty = false;
        refresh();
        clearDataKey();
      } else {
        await projectReportApi.create(postData);
        store.dirty = false;
        to('/agile/project-report');
      }
    }
  }, [baseInfoRef, edit, refresh, store, getPostData]);
  const handlePreview = useCallback(async () => {
    let dataKey;
    if (store.dirty) {
      dataKey = uuidV4();
      const data = await getPostData();
      if (data) {
        dataKeySet.add(dataKey);
        // @ts-ignore
        window[dataKey] = data;
      }
    }
    window.open(`/#${linkUrl(`/agile/project-report/preview/${store.baseInfo?.id}`, {
      type: 'project',
      params: {
        fullPage: 'true',
        dataKey,
      },
    })}`);
  }, [store.baseInfo?.id]);
  // const genPdf = useCallback((canvases: HTMLCanvasElement[]): JsPDF => {
  //   const pdf = new JsPDF('p', 'px');
  //   pdf.deletePage(1);
  //   canvases.forEach((canvas) => {
  //     const contentWidth = canvas.width;
  //     const contentHeight = canvas.height;
  //     pdf.addPage([contentWidth, contentHeight], contentWidth > contentHeight ? 'l' : 'p');
  //     pdf.addImage(canvas, 'png', 0, 0, contentWidth, contentHeight);
  //   });
  //   return pdf;
  // }, []);
  const handleExport = useCallback((canvases: { canvas: HTMLCanvasElement, height: number, width: number }[]) => {
    setExporting(false);
    // 导出pdf
    // const pdf = genPdf(canvases);
    // pdf.save(`${store.baseInfo?.title ?? '项目报告'}.pdf`);
    // 导出html
    const urls = canvases.map((canvas) => canvas.canvas.toDataURL('image/png'));
    const html = htmlTemplate(urls.map((url, i) => ({ url, height: canvases[i].height })));
    const blob = new Blob([html], { type: 'text/html;charset=utf-8' });
    fileSaver.saveAs(blob, `${store.baseInfo?.title ?? '项目报告'}.html`);
  }, [store.baseInfo?.title]);
  const handleExportClick = useCallback(() => {
    setExporting(true);
    exportRef.current?.export(handleExport);
  }, [handleExport]);
  const toFile = useCallback((canvas: HTMLCanvasElement): Promise<File> => new Promise((resolve) => {
    canvas.toBlob((blob) => {
      const file = new File([blob!], `${store.baseInfo?.title ?? '项目报告'}.png`, { type: 'png' });
      resolve(file);
    });
  }), [store.baseInfo?.title]);
  const handleSend = useCallback(async (canvases: { canvas: HTMLCanvasElement, height: number, width: number }[]) => {
    if (store.baseInfo?.id) {
      const formData = new FormData();
      const files = await Promise.all(Array.from(canvases).map((canvas) => toFile(canvas.canvas)));
      files.forEach((file) => {
        formData.append('file', file);
      });
      const urls: string[] = await fileApi.uploadImage(formData);
      const html = htmlTemplate(urls.map((url, i) => ({ url, height: canvases[i].height, width: '100%' })));
      const { objectVersionNumber } = await projectReportApi.send(store.baseInfo?.id, html);
      store.setObjectVersionNumber(objectVersionNumber);
      setSending(false);
      Choerodon.prompt('发送成功', 'success');
    }
    setSending(false);
  }, [store, toFile]);
  const handleSendClick = useCallback(() => {
    setSending(true);
    exportRef.current?.export(handleSend);
  }, [handleSend]);
  const handleCancelClick = useCallback(() => {
    Modal.open({
      title: '确认退出？',
      children: `确认退出${edit ? '编辑' : '创建'}？`,
      onOk: () => {
        // 确认退出后不需要保存提示
        store.setDirty(false);
        to('/agile/project-report');
      },
    });
  }, [edit, store]);
  return (
    <div
      className={styles.bar}
    >
      <Button funcType={'raised' as FuncType} color={'primary' as ButtonColor} onClick={handleSubmit}>{edit ? '保存' : '创建'}</Button>
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
