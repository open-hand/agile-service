import React from 'react';
import { Spin } from 'choerodon-ui/pro';
import { Permission } from '@choerodon/boot';
import { RouteProps } from 'react-router';
import { Route } from 'react-router-dom';
// @ts-ignore
import NoAccess from '@choerodon/master/lib/containers/components/c7n/tools/error-pages/403';
import { Size } from 'choerodon-ui/pro/lib/core/enum';

const defaultChildren = (
  <div style={{
    textAlign: 'center',
    paddingTop: 300,
  }}
  >
    <Spin size={'large' as Size} />
  </div>
);

interface Props extends RouteProps {
  service?: string[]
}
const PermissionRoute: React.FC<Props> = ({ service, component, ...restProps }) => (
  <Permission
    service={service}
    defaultChildren={defaultChildren}
    noAccessChildren={<NoAccess />}
  >
    <Route
      {...restProps}
      component={component}
    />
  </Permission>
);
export default PermissionRoute;
