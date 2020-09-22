import React, {
  useRef, useState, useCallback, useMemo,
} from 'react';
import {
  Page, Breadcrumb, Content,
} from '@choerodon/boot';
import { Button, Dropdown, Menu } from 'choerodon-ui/pro';
import html2canvas from 'html2canvas';
import fileSaver from 'file-saver';
import { IReportContentType } from '@/common/types';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import openAddModal from './components/add-modal';
import BlockList from './components/block-list';
import BaseInfo from './components/base-info';
import Operation from './components/operation';
import PreviewReport from '../report-preview';
import ProjectReportContext, { BaseInfoRef } from './context';
import ProjectReportStore from './store';
import styles from './index.less';
import generateTask from '../report-preview/generateTask';

interface Props {
  store: ProjectReportStore
  edit?: boolean
}
const ReportPage: React.FC<Props> = ({ store, edit }) => {
  const baseInfoRef = useRef<BaseInfoRef>({} as BaseInfoRef);
  const containerRef = useRef<HTMLDivElement>(null);
  const task = useMemo(() => generateTask('export', () => {
    console.log('finish');
    if (containerRef.current) {
      const element = containerRef.current;
      html2canvas(element, {
        allowTaint: true,
        useCORS: true,
        logging: false,
        height: element.scrollHeight,
        windowHeight: element.scrollHeight,
      }).then((canvas) => {
        setExporting(false);
        // const img = canvas.toDataURL();
        canvas.toBlob((blob: Blob) => {
          fileSaver.saveAs(blob, `${'test'}.png`);
        });
      });
    }
  }), []);
  const [preview, setPreview] = useState(false);
  const [exporting, setExporting] = useState(false);
  const handleExport = useCallback(() => {
    setExporting(true);
    // setTimeout(() => {
    //   if (containerRef.current) {
    //     const element = containerRef.current;
    //     html2canvas(element, {
    //       allowTaint: true,
    //       useCORS: true,
    //       logging: false,
    //       height: element.scrollHeight,
    //       windowHeight: element.scrollHeight,
    //     }).then((canvas) => {
    //       setExporting(false);
    //       // const img = canvas.toDataURL();
    //       canvas.toBlob((blob: Blob) => {
    //         fileSaver.saveAs(blob, `${'test'}.png`);
    //       });
    //     });
    //   }
    // });
  }, []);
  return (
    <ProjectReportContext.Provider value={{
      store,
      baseInfoRef,
      edit: edit || false,
      preview,
      setPreview,
      doExport: handleExport,
    }}
    >
      {exporting && <div style={{ position: 'fixed', top: -100000, left: -100000 }}><PreviewReport task={task} innerRef={containerRef} /></div>}
      {preview ? <PreviewReport fullPage /> : (
        <Page>
          <Breadcrumb title={edit ? '编辑项目报告' : '创建项目报告'} />
          <Content style={{ paddingBottom: 0 }}>
            <div className={styles.container}>
              <div className={styles.content}>
                <div className={styles.header}>
                  <div className={styles.tip} />
                  <span className={styles.title}>基本信息</span>
                </div>
                <BaseInfo />
                <div>
                  <div className={styles.header}>
                    <div className={styles.tip} />
                    <span className={styles.title}>报告内容</span>
                    <Dropdown
                      trigger={['click' as Action]}
                      overlay={(
                        <Menu onClick={({ key }) => {
                          openAddModal({
                            type: key as IReportContentType,
                            store,
                          });
                        }}
                        >
                          <Menu.Item key="text">文本</Menu.Item>
                          <Menu.Item key="static_list">静态列表</Menu.Item>
                          <Menu.Item key="dynamic_list">动态列表</Menu.Item>
                          <Menu.Item key="chart">图表</Menu.Item>
                        </Menu>
                      )}
                    >
                      <Button icon="add" color={'blue' as ButtonColor} style={{ marginLeft: 10 }}>
                        添加报告内容
                      </Button>
                    </Dropdown>
                  </div>
                  <BlockList />
                </div>
              </div>
              <Operation />
            </div>
          </Content>
        </Page>
      )}

    </ProjectReportContext.Provider>
  );
};
export default ReportPage;
