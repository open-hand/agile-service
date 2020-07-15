import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import IssueDetailContext from '../../context';

const IssueNum: React.FC = () => {
  const { store: { issue } } = useContext(IssueDetailContext);
  return <span>{issue?.summary}</span>;
};

export default observer(IssueNum);
