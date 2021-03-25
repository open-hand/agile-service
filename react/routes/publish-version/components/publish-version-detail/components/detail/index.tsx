import React from 'react';
import StartDate from './start-date';
import detailStyles from './index.less';
import Section from '../section';
import ExpectReleaseDate from './expect-release-date';
import CreateDate from './create-date';
import ActualDate from './actual-date';
import CreateUser from './create-user';
import Description from './description';
import LinkService from './link-service';
import DependencyTree from './dependency -tree';
import { useReleaseDetailContext } from '../../stores';

const Detail: React.FC = () => (
  <>
    <Section title="详情" border contentClassName={detailStyles.detail}>
      {/* <StartDate /> */}
      {/* <ExpectReleaseDate /> */}
      <ActualDate />
      <CreateDate />
      <CreateUser />
    </Section>
    <Description />
    <LinkService />
    <DependencyTree />
  </>
);
export default Detail;
