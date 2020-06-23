import React, { useContext } from 'react';
import { Button } from 'choerodon-ui/pro';
import IssueDetailContext from '../../context';

const CloseButton: React.FC = () => {
  const { store } = useContext(IssueDetailContext);
  return (
    <Button onClick={() => {
      store.close();
    }}
    >
      关闭
    </Button>
  );
};

export default CloseButton;
