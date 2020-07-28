import React, { useContext } from 'react';
import { Button } from 'choerodon-ui/pro';
import IssueDetailContext from '../../context';
import styles from './index.less';

const CloseButton: React.FC = () => {
  const { store } = useContext(IssueDetailContext);
  return (
    <Button
      className={styles.close}
      onClick={() => {
        store.close();
      }}
      icon="last_page"
    >
      隐藏详情
    </Button>
  );
};

export default CloseButton;
