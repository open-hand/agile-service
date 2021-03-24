import React from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import {
  Button, Tooltip,
} from 'choerodon-ui/pro/lib';
import { observer } from 'mobx-react-lite';
import {
  omit,
} from 'lodash';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';

const TooltipButton: React.FC<{ title?: string } & Omit<ButtonProps, 'title'>> = ({
  title, children, disabled, ...otherProps
}) => {
  if (title && disabled) {
    return <Tooltip title={title}><Button disabled={disabled} {...omit(otherProps, 'onClick')}>{children}</Button></Tooltip>;
  }
  return <Button {...otherProps}>{children}</Button>;
};
function PublishVersion() {
  const prefixCls = '';
  return (
    <Page>
      <Header>
        <TooltipButton
          icon="playlist_add"
          title="无相应权限创建发布版本"
        >
          创建发布版本
        </TooltipButton>
      </Header>
      <Breadcrumb />
      <Content className={`${prefixCls}-content`} style={{ overflowY: 'hidden', display: 'flex', flexDirection: 'column' }}>
        F
      </Content>
    </Page>
  );
}
export default observer(PublishVersion);
