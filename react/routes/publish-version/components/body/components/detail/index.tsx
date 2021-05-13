import React from 'react';
import { observer } from 'mobx-react-lite';
import { usePublishVersionContext } from '../../../../stores';
import PublishVersionSection from '../section';
import Fields from './fields';

function PublishVersionDetail() {
  const { preview } = usePublishVersionContext();

  return (
    <PublishVersionSection border={!preview}>
      <Fields />
    </PublishVersionSection>
  );
}
export default observer(PublishVersionDetail);
