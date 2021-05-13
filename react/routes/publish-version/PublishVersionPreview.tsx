import React from 'react';

import {
  Button, Tooltip, Dropdown, Menu,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import {
  omit,
} from 'lodash';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';
import SideNav from '@/components/side-nav';
import { useParams } from 'react-router';
import Provider, { usePublishVersionContext } from './stores';
import { openCreatePublishVersionModal } from './components/create-edit-publish-version';
import openExportPublishVersionModal from './components/export';
import PublishVersionList from './components/list';
import Container from './Container';
import styles from './PublishVersionPreview.less';
import Header from './components/header';
import Detail from './components/body/components/detail';
import LinkVersion from './components/body/components/link-version';
import IssueTable from './components/body/components/issue-info-table';

const PreviewSection: React.FC<{ title: string }> = ({ title, children }) => (
  <div className={styles.section}>
    <span className={styles.title}>
      <div className={styles.tip} />
      {title}
    </span>
    <div className={styles.section_body}>
      {children}
    </div>
  </div>
);

function PublishVersion() {
  const { prefixCls, store, tableDataSet } = usePublishVersionContext();

  return (
    <div className={styles.wrap}>
      <Header />
      <div className={styles.body}>
        <PreviewSection title="详情">
          <Detail />
        </PreviewSection>
        <PreviewSection title="关联版本">
          <LinkVersion />
        </PreviewSection>
        <PreviewSection title="版本信息">
          <IssueTable />
        </PreviewSection>
      </div>
    </div>
  );
}
const ObserverPublishVersion = observer(PublishVersion);
export default function Index(props: any) {
  const params = useParams() as any;
  console.log('params..', params);
  return (
    <Provider {...props} publishVersionId={params.id} preview>
      <ObserverPublishVersion />
    </Provider>
  );
}
