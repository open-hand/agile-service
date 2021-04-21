import React from 'react';
import ArtifactId from './artifactId';
import detailStyles from './index.less';
import Section from '../section';
import ExpectReleaseDate from './expect-release-date';
import CreateDate from './create-date';
import ActualDate from './actual-date';
import CreateUser from './create-user';
import Description from './description';
import GroupId from './groupId';
import AppService from './app-service';
import TagField from './tag-field';
import DependencyTree from './dependency -tree';
import { useReleaseDetailContext } from '../../stores';

const Detail: React.FC = () => (
  <>
    <Section title="详情" border contentClassName={detailStyles.detail}>
      {/* <ArtifactId /> */}
      {/* <ExpectReleaseDate /> */}
      <ActualDate />
      <ArtifactId />
      <GroupId />
      <AppService />
      <TagField />
      <CreateDate />
      <CreateUser />
    </Section>
    <Description />
    <DependencyTree />
  </>
);
export default Detail;