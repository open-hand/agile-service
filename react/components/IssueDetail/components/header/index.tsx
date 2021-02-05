import React from 'react';
import { observer } from 'mobx-react-lite';
import TypeTag from '../type-tag';
import ParentSummary from '../parent-summary';
import Numbering from '../numbering';
import Summary from '../fields/summary';
import CloseButton from '../close-button';
import styles from './index.less';
import { useDetailContext } from '../../context';

const Header: React.FC = () => {
  const {
    closeButton = true,
  } = useDetailContext();
  return (
    <div
      className={styles.header}
    >
      <div className={styles.line}>
        <TypeTag />
        <ParentSummary />
        <Numbering />
        {closeButton && <CloseButton />}
      </div>
      <div className={styles.line} style={{ alignItems: 'flex-start' }}>
        <Summary />
      </div>
    </div>
  );
};

export default observer(Header);
