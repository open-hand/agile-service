// @ts-nocheck
import React from 'react';
import { observer } from 'mobx-react-lite';
import { useParams } from 'react-router';
import Provider from './stores';
import styles from './PublishVersionPreview.less';
import Header from './components/header';
import Detail from './components/body/components/detail';
// import LinkVersion from './components/body/components/link-version';
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
  return (
    <div className={styles.wrap}>
      <Header />
      <div className={styles.body}>
        <PreviewSection title="详情">
          <Detail />
        </PreviewSection>
        <PreviewSection title="关联版本">
          {/* <LinkVersion /> */}
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
  return (
    <Provider {...props} publishVersionId={params.id} preview>
      <ObserverPublishVersion />
    </Provider>
  );
}
