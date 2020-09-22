import React, { useEffect } from 'react';
import BaseInfo from '../report-page/components/base-info';
import BlockList from '../report-page/components/block-list';
import styles from './index.less';

const PreviewReport:React.FC = () => {
  if (!document.body.classList.contains('hidden')) {
    document.body.classList.add('hidden');
  }
  useEffect(() => () => {
    document.body.classList.remove('hidden');
  }, []);
  return (
    <div className={styles.preview}>
      <BaseInfo preview />
      <div style={{
        height: 1,
        background: '#3F51B5FF',
        margin: '30px 0 20px 0',
      }}
      />
      <div>
        <BlockList preview />
      </div>
    </div>
  );
};

export default PreviewReport;
