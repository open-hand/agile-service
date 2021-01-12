import React from 'react';
import { observer } from 'mobx-react-lite';
import { useDetailContext } from '../../context';
import styles from './index.less';

const Numbering: React.FC = () => {
  const { store: { issue } } = useDetailContext();
  return <span className={styles.numbering}>{issue?.issueNum || '-'}</span>;
};

export default observer(Numbering);
