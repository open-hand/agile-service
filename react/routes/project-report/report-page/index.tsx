import React, { useRef, useState, useMemo } from 'react';
import {
  Page, Breadcrumb, Content, Header, stores,
} from '@choerodon/boot';
import { Button, Dropdown, Menu } from 'choerodon-ui/pro';
import { IReportContentType } from '@/common/types';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import useIsProgram from '@/hooks/useIsProgram';
import openAddModal from './components/add-modal';
import BlockList from './components/block-list';
import BaseInfo from './components/base-info';
import Operation from './components/operation';
import PreviewReport from '../report-preview/Preview';
import ProjectReportContext, { BaseInfoRef } from './context';
import ProjectReportStore from './store';
import styles from './index.less';

const { AppState } = stores;
interface Props {
  store: ProjectReportStore
  edit?: boolean
  preview?: boolean
  refresh?: () => void
}
const noop = () => {};
const ReportPage: React.FC<Props> = ({
  store, edit, preview: forcePreview, refresh,
}) => {
  const baseInfoRef = useRef<BaseInfoRef>({} as BaseInfoRef);
  const [preview, setPreview] = useState(forcePreview !== undefined ? forcePreview : false);
  const { isProgram } = useIsProgram();
  const addBlock = useMemo(() => (
    <Dropdown
      trigger={['click' as Action]}
      overlay={(
        <Menu
          onClick={({ key }) => {
            openAddModal({
              type: key as IReportContentType,
              store,
            });
          }}
          selectable={false}
        >
          <Menu.Item key="text">文本</Menu.Item>
          {!isProgram && [
            <Menu.Item key="static_list">静态列表</Menu.Item>,
            <Menu.Item key="dynamic_list">动态列表</Menu.Item>,
          ]}
          <Menu.Item key="chart">图表</Menu.Item>
        </Menu>
      )}
    >
      <Button icon="add" color={'blue' as ButtonColor}>
        添加报告内容
      </Button>
    </Dropdown>
  ), [isProgram, store]);
  return (
    <ProjectReportContext.Provider value={{
      store,
      baseInfoRef,
      edit: edit || false,
      preview,
      setPreview,
      refresh: refresh || noop,
    }}
    >
      {preview ? <PreviewReport /> : (
        <Page>
          <Header>
            {addBlock}
          </Header>
          <Breadcrumb title={edit ? '编辑项目报告' : '创建项目报告'} />
          <Content style={{ paddingBottom: 0, paddingTop: 0 }}>
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
                    {addBlock}
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
