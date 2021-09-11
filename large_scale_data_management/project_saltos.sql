/*Load the data*/
/*\< C:\Users\thomas\ownCloud\data science\Large scale data management\project\labels.sql
\< C:\Users\thomas\ownCloud\data science\Large scale data management\project\specs.sql*/

/*Add primary and foreign key */
/*ALTER TABLE sys.specs ADD PRIMARY KEY ("spec id");
ALTER TABLE sys.labels ADD FOREIGN KEY ("left spec id") REFERENCES sys.specs("spec id");
ALTER TABLE sys.labels ADD FOREIGN KEY ("right spec id") REFERENCES sys.specs("spec id");*/

/*Check the keys*/
/*select t.name as table_name
	,c.name as column_name
	,c.type as data_type
	,c."null" as IsNULL
	,k.key_name
from sys.columns c
	inner join sys.tables t on c.table_id = t.id
	left join sys.dependency_columns_on_keys k on k.column_id = c.id
where t.name = 'specs';

SELECT * FROM sys.dependency_tables_on_foreignkeys;*/


/*Create UDF */
DROP FUNCTION cossim_saltos(STRING, STRING);
CREATE Function cossim_saltos(titles STRING, query STRING)
RETURNS FLOAT
LANGUAGE PYTHON {
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.metrics.pairwise import cosine_similarity
vectorizer = CountVectorizer(lowercase=True, max_df=0.5, min_df=10, token_pattern = r'\b[a-zA-Z]{3,}\b', stop_words = "english") 
train_data_features = vectorizer.fit_transform(titles)
q_v = vectorizer.transform([query]).toarray()
return cosine_similarity(train_data_features, q_v).ravel()
};

/*queries*/
select cossim_saltos("page title", 'Nikon d500') from specs;
SELECT "spec id" FROM specs WHERE cossim_saltos("page title", 'Nikon d500') > 0.8;
SELECT "spec id" FROM specs WHERE cossim_saltos("page title", 'Polaroid Is426') > 0.7;
SELECT "spec id" FROM specs WHERE cossim_saltos("page title", ' Canon IXUS 310 HS 12.1 Megapixel') > 0.6;
