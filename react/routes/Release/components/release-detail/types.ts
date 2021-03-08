interface IReleaseDetailData {
  id: string
  name: string
  startDate: string
  expectReleaseDate: string
  statusCode: 'version_planning' | 'released' | 'archived'
  statusName: string
}
export default IReleaseDetailData;
