import React, { useRef } from 'react';
import {
  Page, Header, Breadcrumb, Content,
} from '@choerodon/boot';
import { Button, Dropdown, Menu } from 'choerodon-ui/pro';
import { IReportContentType } from '@/common/types';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import openAddModal from './components/add-modal';
import BlockList from './components/block-list';
import BaseInfo from './components/base-info';
import Operation from './components/operation';
import ProjectReportContext, { BaseInfoRef } from './context';
import ProjectReportStore from './store';
import styles from './index.less';

interface Props {
  store: ProjectReportStore
  edit?: boolean
}
const ReportPage: React.FC<Props> = ({ store, edit }) => {
  const baseInfoRef = useRef<BaseInfoRef>({} as BaseInfoRef);
  return (
    <Page>
      {/* <Header>
        <Button icon="playlist_add">添加报告内容</Button>
      </Header> */}
      <Breadcrumb title={edit ? '编辑项目报告' : '创建项目报告'} />
      <Content style={{ paddingBottom: 0 }}>
        <ProjectReportContext.Provider value={{
          store,
          baseInfoRef,
          edit: edit || false,
        }}
        >
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
                        <Menu.Item key="list">动态列表</Menu.Item>
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
        </ProjectReportContext.Provider>
      </Content>
    </Page>
  );
};
export default ReportPage;
