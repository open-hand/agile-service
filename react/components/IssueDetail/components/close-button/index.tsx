import React from 'react';
import { Button } from 'choerodon-ui/pro';
import { useDetailContext } from '../../context';
import styles from './index.less';

const CloseButton: React.FC = () => {
  const { store } = useDetailContext();
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
