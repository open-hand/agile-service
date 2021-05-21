import React from 'react';
import { observer } from 'mobx-react-lite';
import { usePublishVersionContext } from '../../../../stores';
import PublishVersionSection from '../section';
import Fields from './fields';
import styles from './index.less';

function PublishVersionDetail() {
  const { preview } = usePublishVersionContext();

  return (
    <PublishVersionSection border={!preview} bodyClassName={styles.section_body}>
      <Fields />
    </PublishVersionSection>
  );
}
export default observer(PublishVersionDetail);
