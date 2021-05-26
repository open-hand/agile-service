import React from 'react';
import { observer } from 'mobx-react-lite';
import { usePublishVersionContext } from '../../../../stores';
import PublishVersionSection from '../section';
import Fields from './fields';
import styles from './index.less';

function PublishVersionDetail({ customFields }: { customFields?: any[] }) {
  const { preview, menuDetail } = usePublishVersionContext();

  return (
    <PublishVersionSection border={!preview} bodyClassName={styles.section_body}>
      <Fields customFields={customFields} />
    </PublishVersionSection>
  );
}
PublishVersionDetail.defaultProps = {
  customFields: undefined,
};
export default observer(PublishVersionDetail);
