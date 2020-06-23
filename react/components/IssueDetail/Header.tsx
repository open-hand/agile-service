import React from 'react';
import styles from './Header.less';
import IssueNum from './components/issue-num';
import CloseButton from './components/close-button';

const Header: React.FC = () => (
  <div
    className={styles.header}
  >
    <IssueNum />
    <CloseButton />
  </div>
);

export default Header;
