import React, { useCallback } from 'react';
import { Button } from 'choerodon-ui/pro';
import openColumnManageModal, { ColumnManageProps } from './Modal';

const ColumnManage: React.FC<ColumnManageProps> = (props) => {
  const handleClick = useCallback(() => {
    openColumnManageModal(props);
  }, [props]);
  return (
    <div>
      <Button icon="view_column" onClick={handleClick} />
    </div>
  );
};

export default ColumnManage;
