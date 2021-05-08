import React from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import {
  Button, Menu, Table, Tooltip, Modal,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import {
  omit,
} from 'lodash';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';
import TableDropMenu from '@/common/TableDropMenu';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { publishVersionApi, versionApi } from '@/api';
import VERSION_STATUS_TYPE from '@/constants/VERSION_STATUS_TYPE';
import { usePublishVersionContext } from '../../../../stores';

import './PublishVersion.less';
import PublishVersionSection from '../section';
import Fields from './fields';

const { Column } = Table;

function PublishVersionDetail() {
  const { prefixCls, tableDataSet } = usePublishVersionContext();

  return (
    <PublishVersionSection>
      详情
      <Fields />
    </PublishVersionSection>
  );
}
export default observer(PublishVersionDetail);
