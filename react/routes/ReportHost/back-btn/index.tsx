import React from 'react';
import { Button, Tooltip } from 'choerodon-ui/pro';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/interface';
import { useHistory } from 'react-router';
import { linkUrl } from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';

const BackBtn = (props?: ButtonProps) => {
  const history = useHistory();
  return (
  // <Tooltip title="返回">
    <Button
      {...props}
      icon="arrow_back"
      onClick={() => {
        history.push(linkUrl(LINK_URL.report));
      }}
    >
      返回
    </Button>
  // </Tooltip>
  );
};

export default BackBtn;
