import React from 'react';
import StartDate from './start-date';
import detailStyles from './index.less';
import Section from '../section';

const Detail: React.FC = () => (
  <div className={detailStyles.detail}>
    <Section title="详情" border style={{ marginLeft: 20, marginRight: 20 }}>
      <StartDate />
    </Section>

    <Section title="描述" border style={{ marginLeft: 20, marginRight: 20 }}>
      00
    </Section>
    <Section title="关联应用版本" border style={{ marginLeft: 20, marginRight: 20 }}>
      关联应用版本
    </Section>
  </div>

);
export default Detail;
