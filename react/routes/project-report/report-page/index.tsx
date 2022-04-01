import React, {
  useRef, useState, useMemo, useCallback,
} from 'react';
import {
  Page, Breadcrumb, Content, Header, HeaderButtons,
} from '@choerodon/boot';
import { Button, Dropdown, Menu } from 'choerodon-ui/pro';
import { IReportContentType } from '@/common/types';
import { ButtonProps, ButtonColor } from 'choerodon-ui/pro/lib/button/interface';
import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import useIsProgram from '@/hooks/useIsProgram';
import openAddModal from './components/add-modal';
import openAutoSendModal from './components/auto-send';
import BlockList from './components/block-list';
import BaseInfo from './components/base-info';
import Operation from './components/operation';
import CollapseAll from './components/collapse-all';
import PreviewReport from '../report-preview/Preview';
import ProjectReportContext, { BaseInfoRef } from './context';
import ProjectReportStore from './store';
import styles from './index.less';

interface Props {
  store: ProjectReportStore
  edit?: boolean
  preview?: boolean
  refresh?: () => void
}
const noop = () => { };
const ReportPage: React.FC<Props> = ({
  store, edit, preview: forcePreview, refresh,
}) => {
  const baseInfoRef = useRef<BaseInfoRef>({} as BaseInfoRef);
  const [preview, setPreview] = useState(forcePreview !== undefined ? forcePreview : false);
  const { isProgram } = useIsProgram();
  const renderAddButton = useCallback((props?: ButtonProps) => (
    <Dropdown
      trigger={['click' as Action]}
      overlay={(
        <Menu
          style={{ width: 120 }}
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
      <Button {...props} icon="playlist_add">
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
            <HeaderButtons items={[{
              display: true,
              element: renderAddButton({ color: 'primary' as ButtonColor }),
            // }, {
            //   display: true,
            //   name: '设置定时发报',
            //   icon: 'settings',
            //   handler: () => {
            //     openAutoSendModal({});
            //   },
            }, {
              display: true,
              element: <CollapseAll />,
            }]}
            />
          </Header>
          <Breadcrumb title={edit ? '修改项目报告' : '创建项目报告'} />
          <Content style={{
            paddingBottom: 0,
            // paddingTop: 0,
            overflow: 'hidden',
            display: 'flex',
            flexDirection: 'column',
            paddingTop: 20,
          }}
          >
            {/* <div className={styles.container}> */}
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
                  {renderAddButton()}
                </div>
                <BlockList />
              </div>
            </div>
            <Operation />
            {/* </div> */}
          </Content>
        </Page>
      )}

    </ProjectReportContext.Provider>
  );
};
export default ReportPage;
