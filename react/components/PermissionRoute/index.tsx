import React from 'react';
import { Permission } from '@choerodon/boot';
import { RouteProps } from 'react-router';
import { Route } from 'react-router-dom';
// @ts-ignore
import NoAccess from '@choerodon/master/lib/containers/components/c7n/tools/error-pages/403';

interface Props extends RouteProps {
  service?: string[]
}
const PermissionRoute: React.FC<Props> = ({ service, component, ...restProps }) => (
  <Permission
    service={service}
  >
    {(hasPermission: boolean) => (
      <Route
        {...restProps}
        component={hasPermission ? component : NoAccess}
      />
    )}
  </Permission>
);
export default PermissionRoute;
